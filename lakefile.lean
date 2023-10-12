import Lake
open Lake DSL

package «LeanBlueprintExample» where
  -- add any package configuration options here

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"

@[default_target]
lean_lib «LeanBlueprintExample» where
  -- add any library configuration options here

lean_exe «decls» where
  root := `exe.Decls
