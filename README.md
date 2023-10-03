# LeanBlueprintExample

1. Install elan

2. Create your project on command line using something like (replace all "YourProject" to your project name in all the following commands)

```
lake +leanprover-community/mathlib4:lean-toolchain new YourProject math
```

3. Add `doc-gen4` dependency to `lakefile.lean` following [](https://github.com/leanprover/doc-gen4#usage)

4. Enter `YourProject` then run the following on command line:

```
lake update
lake exe cache get
lake build
```

5. Copy the dummy file `LeanBlueprintExample/Basic.lean` from `LeanBlueprintExample` to `YourProject/YourProject/Basic.lean`

6. Copy everything in `blueprint`, `.github` from `LeanBlueprintExample` to your project and change every "LeanBlueprintExample" to "YourProject"

7. Run the following to test locally:

```
lake -Kenv=dev update
lake -Kenv=dev build YourProject:docs
```