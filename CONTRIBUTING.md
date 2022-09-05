# Contributing to the `cardano-node` project

## Updating dependencies

### ... from Hackage

Updating package dependencies from Hackage should work like normal in a Haskell project.
The most important thing to note is that we pin the `index-state` of the Hackage package index in `cabal.project`.
This means that cabal will always see Hackage "as if" it was that time, ensuring reproducibility.
But it also means that if you need a package version that was released *after* that time, you need to bump the `index-state` (and to run `cabal update` locally).

Because of how we use Nix to manage our Haskell build, whenever you do this you will also need to pull in the Nix equivalent of the newer `index-state`.
You can do this by running `nix flake lock --update-input hackageNix`.

### ... from the Cardano pacakge repository

Many Cardano packages are not on Hackage and are instead in the [Cardano package repository](https://github.com/input-output-hk/cardano-haskell-package-repo), see the README for more information.
Getting new packages from there works much like getting them from Hackage.
The differences are that it has an independent `index-state`, and that there is a different Nix command you need to run afterwards: `nix flake lock --update-input cardanoHaskellPackageRepo`.

## Patching dependencies

Sometimes we need to fix an issue in one of our dependencies that is not under our control.
There are several options for what to do, here they are in order of preference:

1. Fix the issue upstream, get the maintainer to release to Hackage, use that.
2. If it will be some time before a release, use a `source-repository-package` stanza to pull in the patched version. 
Try only to do this for short-lived forks, as it does not play very well with tooling.
3. If the fork looks like it will be long-lived or permanent, release a patched version to the [Cardano package repository](https://github.com/input-output-hk/cardano-haskell-package-repo).
See the README for instructions.

## Releasing a version of the node

(There is much more to say here, this is just a small fragment)

### ... to the Cardano package repository

When releasing a new version of the node, it and the other packages in this repository should be released to the [Cardano package repository](https://github.com/input-output-hk/cardano-haskell-package-repo).
See the README for instructions, including a script to automate most of the process. Please note that libraries need bounds on the version of their dependencies to avoid bitrot and be effectively reusable.
