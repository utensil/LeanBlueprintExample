import Lean

import Lake
import Lake.Load

open Lean System

open Lake

-- ## From DocGen4 lakefile

/--
Turns a Github git remote URL into an HTTPS Github URL.
Three link types from git supported:
- https://github.com/org/repo
- https://github.com/org/repo.git
- git@github.com:org/repo.git
TODO: This function is quite brittle and very Github specific, we can
probably do better.
-/
def getGithubBaseUrl (gitUrl : String) : String := Id.run do
  let mut url := gitUrl
  if url.startsWith "git@" then
    url := url.drop 15
    url := url.dropRight 4
    return s!"https://github.com/{url}"
  else if url.endsWith ".git" then
    return url.dropRight 4
  else
    return url

/--
Obtain the Github URL of a project by parsing the origin remote.
-/
def getProjectGithubUrl (directory : System.FilePath := "." ) : IO String := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["remote", "get-url", "origin"],
    cwd := directory
  }
  if out.exitCode != 0 then
    throw <| IO.userError <| s!"git exited with code {out.exitCode} while looking for the git remote in {directory}"
  return out.stdout.trimRight

/--
Obtain the git commit hash of the project that is currently getting analyzed.
-/
def getProjectCommit (directory : System.FilePath := "." ) : IO String := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["rev-parse", "HEAD"]
    cwd := directory
  }
  if out.exitCode != 0 then
    throw <| IO.userError <| s!"git exited with code {out.exitCode} while looking for the current commit in {directory}"
  return out.stdout.trimRight

def getGitUrl (pkg : Package) (lib : LeanLibConfig) (mod : Lake.Module) : IO String := do
  let baseUrl := getGithubBaseUrl (← getProjectGithubUrl pkg.dir)
  let commit ← getProjectCommit pkg.dir

  let parts := mod.name.components.map toString
  let path := String.intercalate "/" parts
  let libPath := pkg.config.srcDir / lib.srcDir
  let basePath := String.intercalate "/" (libPath.components.filter (· != "."))
  let url := s!"{baseUrl}/blob/{commit}/{basePath}/{path}.lean"
  return url

-- ## From Cache.IO

def LAKEPACKAGESDIR : FilePath :=
  ⟨"lake-packages"⟩

/-- Whether this is running on Mathlib repo or not -/
def isMathlibRoot : IO Bool :=
  FilePath.mk "Mathlib" |>.pathExists

def mathlibDepPath : FilePath :=
  LAKEPACKAGESDIR / "mathlib"

abbrev PackageDirs := Lean.RBMap String FilePath compare

def getPackageDirs : IO PackageDirs := return .ofList [
  ("Mathlib", if ← isMathlibRoot then "." else mathlibDepPath),
  ("MathlibExtras", if ← isMathlibRoot then "." else mathlibDepPath),
  ("Archive", if ← isMathlibRoot then "." else mathlibDepPath),
  ("Counterexamples", if ← isMathlibRoot then "." else mathlibDepPath),
  ("Aesop", LAKEPACKAGESDIR / "aesop"),
  ("Std", LAKEPACKAGESDIR / "std"),
  ("Cli", LAKEPACKAGESDIR / "Cli"),
  ("ProofWidgets", LAKEPACKAGESDIR / "proofwidgets"),
  ("Qq", LAKEPACKAGESDIR / "Qq")
  -- TODO: allow user extensions for these maps, or generate this map by lake
]

initialize pkgDirs : PackageDirs ← getPackageDirs

-- rewritten to accept Name instead of FilePath
def getPackageDir (module : Name) : Option FilePath :=
  let root := module.getRoot
  let pkg := root.toString
  match pkgDirs.find? pkg with
    | none => none
    | some path => return path

-- ## From DocGen4.Output.SourceLinker
-- modified to add heuristics for Mathlib and current project

/--
Given a lake workspace with all the dependencies as well as the hash of the
compiler release to work with this provides a function to turn names of
declarations into (optionally positional) Github URLs.
-/
def sourceLinker (module : Name) (range : Option DeclarationRange): IO String := do
    let parts := module.components.map Name.toString
    let path := String.intercalate "/" parts
    let root := module.getRoot
    let leanHash := Lean.githash
    let basic <- if root == `Lean ∨ root == `Init then
      pure s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/{path}.lean"
    else if root == `Lake then
      pure s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/lake/{path}.lean"
    else
      -- TODO: how to cache?
      if let some packageDir := getPackageDir root then
        let projectGithubUrl := (← getProjectGithubUrl packageDir)
        let baseUrl := getGithubBaseUrl projectGithubUrl
        let commit := (<-getProjectCommit packageDir)
        pure s!"{baseUrl}/blob/{commit}/{path}.lean"
      else
        pure s!""

    match range with
    | some range => pure s!"{basic}#L{range.pos.line}-L{range.endPos.line}"
    | none => pure basic

-- ## From DocGen4.Load

def envOfImports (imports : Array Name) : IO Environment := do
 importModules (imports.map (Import.mk · false)) Options.empty

-- ## From DocGen4.Process.Hierarchy

def Lean.HashSet.fromArray [BEq α] [Hashable α] (xs : Array α) : Lean.HashSet α :=
  xs.foldr (flip .insert) .empty

-- ## Main code for getting declaration info

structure DeclarationInfo where
  root : Name
  moduleName : Name
  declarationName : Name
  declarationRange : DeclarationRange

  deriving Inhabited, Repr

instance : ToString DeclarationInfo := 
  ⟨reprStr⟩

def getdeclarationInfo (moduleName : Name) (nc : Name × ConstantInfo) : MetaM (Option DeclarationInfo) := do
  let (name, _cinfo) := nc
  let declarationRange <- findDeclarationRanges? name
  let info := match declarationRange with
  | some declarationRange =>
    let range := declarationRange.range
    some {
      root := moduleName.getRoot,
      moduleName := moduleName,
      declarationName := name,
      declarationRange := range
    }
  | none => none

  return info
  
def decls (relevantModules : HashSet Name) : MetaM Unit := do
  let env ← getEnv

  for (name, cinfo) in env.constants.toList do
    let some modidx := env.getModuleIdxFor?  name | unreachable!
    let moduleName := env.allImportedModuleNames.get! modidx
    if !relevantModules.contains moduleName then
      continue

    let config : Core.Context := {
      maxHeartbeats := 5000000,
      options := ← getOptions,
      fileName := ← getFileName,
      fileMap := ← getFileMap
    }

    let analysis ← Prod.fst <$> Meta.MetaM.toIO (getdeclarationInfo moduleName (name, cinfo)) config { env := env } {} {}

    match analysis with
    | some info =>
      let url <- sourceLinker info.moduleName info.declarationRange
      println! s!"{info.declarationName} : {url}"
    | none => continue

  -- IO.FS.writeFile "decls.yaml" ""
  return ()

-- ## Unused code

def getWs : IO Workspace := do
  let (elanInstall?, leanInstall?, lakeInstall?) ← Lake.findInstall?

  if let some leanInstall := leanInstall? then
    if let some lakeInstall := lakeInstall? then
      let env <- Env.compute lakeInstall leanInstall elanInstall?
      let cwd <- IO.currentDir
      let config : LoadConfig := { env := env, rootDir := cwd }
      println! s!"Loading workspace from {cwd}"
      loadWorkspace config |>.ignoreLog.toIO fun e => IO.userError s!"Failed to load workspace: {e}"
    else
      throw <| IO.userError "Lake is not installed"
  else
    throw <| IO.userError "Lean is not installed"

-- ## Main: entry point, misc setup

def main (args : List String) : IO Unit := do
  Lean.initSearchPath (<- Lean.findSysroot)

  if args.isEmpty then
    throw <| IO.userError "Please provide at least one package name"
  else
    pure ()

  -- let packages : Array Name := #["LeanBlueprintExample"]
  let packages : Array Name := args.map Name.mkSimple |>.toArray
  let env <- envOfImports packages
  let relevantModules := HashSet.fromArray env.header.moduleNames

  let config := {
    -- TODO: parameterize maxHeartbeats
    maxHeartbeats := 100000000,
    options := ⟨[(`pp.tagAppFns, true)]⟩,
    -- TODO: Figure out whether this could cause some bugs
    fileName := default,
    fileMap := default,
  }

  Prod.fst <$> Meta.MetaM.toIO (decls relevantModules) config { env := env } {} {}
