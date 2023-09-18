# RichEnv

[![Tests](https://github.com/DavSanchez/richenv/actions/workflows/tests.yml/badge.svg)](https://github.com/DavSanchez/richenv/actions/workflows/tests.yml)
![Hackage Version](https://img.shields.io/hackage/v/:richenv?style=round-square&logo=haskell)
[![nixpkgs 23.05](https://img.shields.io/badge/nixpkgs-unstable-blue.svg?style=round-square&logo=NixOS&logoColor=white)](https://search.nixos.org/packages?size=1&show=richenv)

Rich environment variable setup for Haskell

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

This package exposes a type that captures a set of rules that modify an existing environment variable set, be it a provided list of key-value pairs (list of tuples) or the system's environment variable set. The rules are applied in order, and each rule can be either a prefix, a mapping or a value. Prefixes take environment variable names and prepend a prefix to them, replacing existing prefixes (i.e. fist removing old prefix, then adding the new one) if desired. Mappings replace the name of the environment variable with a different one, and values create the environment variable with the provided value.

## Getting started

The idea behind this library is that you can find a set of rules for setting environment variables that may or may not use the current environment as starting stage, to replace the ones in the current process or pass a custom env to [`System.Process.CreateProcess`](https://hackage.haskell.org/package/process/docs/System-Process.html#t:CreateProcess) to spawn some sub-process.

If your application uses a configuration file, for example in YAML format, you could add a new field to your config like this:

```yaml
# Other configs ...
env:
  values:
    VERBOSE: "true"
    FOO: bar
  mappings:
    NEWNAME: OLDNAME
    NEW_NAME_2: OLD_NAME_2
  prefixes:
    NEW_PREFIX_:
      - OLD_PREFIX_
      - OTHER_OLD_PREFIX_
    OTHER_NEW_PREFIX_: [OTHER_OLD_PREFIX_]
# More configs ...
```

When parsing this new `env` field as the `RichEnv` type (it provides `FromJSON`/`ToJSON` instances), this defines a set of rules:

- `values`: these are simple environment variable definitions with a value (in textual format).
- `mappings`: these will create new environment variables from existing environment variables on an 1-1 basis. In the YAML config above, a `NEWNAME` var will be created with the contents of the `OLDNAME` var.
- `prefixes`: these will create new environment variables from existing environment variable by prefix substitution. In the example, environment variables with the prefixes `OLD_PREFIX_*` and `OTHER_OLD_PREFIX_*` will all be stripped of the prefix and created with the `NEW_PREFIX_*` instead.

Thus, after parsing, you will end up with a set of environment variables that you can:

- Apply to an externally provided list of environment variables and values and then apply the result them to the current process with functions like `setRichEnv`.
- Generate an environment variable list of type `[(Text, Text)]` with `toEnvList`.
- Generate a `[(String, String)]` (with something like `fromEnvironment . toEnvList`) to pass to `System.Process.CreateProcess`.
- etc

You can either provide a list of environment variables (normally of type `[(Text, Text)]`) to apply `RichEnv` rules or use the environment variables from the current process.

### Code example

If you assume that there are no environment variables in the current process, you could use `RichEnv` to get a list of environment variables like this:

```haskell

richEnv :: RichEnv
richEnv = RichEnv
      { prefixes = Prefixes $ HM.singleton "NEW_" ["PREFIXED_"],
        mappings = Mappings $ HM.singleton "SOME" "FOO",
        values = Values $ HM.singleton "OTHER" "othervar"
      }

main :: IO ()
main = do
  mapM_ (uncurry setEnv) [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]
  envList <- toEnvList richEnv
  -- ...
```

And then call a process providing a generated custom environment doing something like:

```haskell
  -- ... later in the `main` function... 
  let envProcess = (proc "env" []) {env = Just (fromEnvironment envList), std_out = CreatePipe}
  out <- lines <$> readCreateProcess envProcess mempty
  -- ... or if inside a test with HSpec
  sort out `shouldBe` sort ["NEW_VAR=content", "NEW_VAR2=content2", "OTHER=othervar", "SOME=bar"]
```

See the documentations (and [the tests](./test/RichEnvSpec.hs)) for more details.
