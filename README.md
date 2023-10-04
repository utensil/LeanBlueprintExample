# LeanBlueprintExample

1. [Install elan](https://github.com/leanprover/elan#installation)

2. Create your project on command line using something like (replace all "YourProject" to your project name in all the following commands)

```
lake +leanprover-community/mathlib4:lean-toolchain new YourProject math
```

which ensures that your project uses the same Lean version as mathlib and will be able to use mathlib as a dependency.

3. Add `doc-gen4` dependency to `lakefile.lean` following [doc-gen4 Usage](https://github.com/leanprover/doc-gen4#usage):

```
meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
```

4. Change the current directory to `YourProject` then run the following on command line:

```
lake update
lake exe cache get
lake build
```

5. Copy the dummy file `LeanBlueprintExample/Basic.lean` from `LeanBlueprintExample` to `YourProject/YourProject/Basic.lean`

6. Copy everything in `blueprint`, `.github` from `LeanBlueprintExample` to your project and change every "LeanBlueprintExample" to "YourProject", also make adjustments to suit your project.

7. Run the following to test locally:

```
# Installs doc-gen4
lake -Kenv=dev update
# Builds docs to build/doc
lake -Kenv=dev build YourProject:docs
# Builds blueprint website to blueprint/web
inv all
```

8. Push to GitHub and enable GitHub Pages on `gh-pages` branch following [Configuring a publishing source for your GitHub Pages site](https://docs.github.com/en/pages/getting-started-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site).

9. CI will run around 10 minutes at `https://github.com/<your-github-username>/YourProject/actions` , when it turns green, check out your blueprint at `https://<your-github-username>.github.io/YourProject/blueprint` and your doc at `https://<your-github-username>.github.io/YourProject/doc`, then definitions and theorems in the blueprint should be able to jump to the corresponding doc pages, for example, the definitions of `one`, `two` and the theorem of `one_plus_one_eq_two` in the demo.
