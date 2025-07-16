
## Elevator pitch
Jenga is a powerful, language agnostic build tool, providing safe, fast, incremental builds for anything!

It's defining features are:
1. Sandboxing.
2. Full history shared caching.
3. Powerful yet simple configuration.

Jenga was originally developed at Jane Street. [More information about the history](doc/hisory.md)

Skip the why, and jump directly to [getting started](tutorial/01_getting_started.md).


## Jenga's features
Here is more detail on Jenga's primary features.

- General Purpose:
Jenga is a entirely language agnostic.
Build rules can be constructed in Jenga's make-style DSL for anything which can be run at the command line.
These rules can be written on a per-project basis or shared more widely,

Many many programming languages (scala, rust, golang, haskell) come with integrated, language-specific build tooling.
This is nice right up until your project needs build automation for other stuff: other languages, test frameworks, documentation, deployments, etc.


- Sandboxing:
Jenga's builds are fully sandboxed.
Every build action is run in a hermetic box allowing access only to declared dependencies.
This ensure no dependencies can be missed, which would lead to incorrect rebuilds.
This is the core of Jenga's safety claim.

- Caching:
Jenga's provides full history shared caching.
Every build at every source version is cached across all projects and repos.
So long as the cache exists, no build action will ever be repeated.
This forms the basic of Jenga's fast, incremental rebuilds.

- Powereful:
Neil Mitchell (author of Shake and major contributor to Buck2) describes monadic dependencies as ["the most important thing shake got right"](https://neilmitchell.blogspot.com/2021/09/reflecting-on-shake-build-system.html).
The core interface to Jenga's build engine is a [simple haskell EDSL](src/Interface.hs) to describe build rules,
This provides dynamic rule generation, and dynamic dependencies (also known as monadic dependencies).

- Simple:
The core EDSL is exposed to the Jenga operator as a [familiar _make-style_ DSL.](doc/jenga-syntax.md).
A simple rule is expressed in this DSL as the triple of: _targets_ `:` _dependencies_ `:` _action_.
Dynamic rules generation is provided by `include`.
Dynamic dependencies are expressed using a simple syntax (`@`) for scanner dependencies.

- Parallel:
Jenga supports parallel builds via the `-jNUM` command line option.
Or by simply running multiple insances of `jenga` at the command line.

- Watcher mode:
Jenga currently has no integrated support,
but a rudimentary watcher mode is possible via `inotifywait`,
as provided by the [`jenga-w`](jenga-w]) wrapper script.

## Prerequisites

To build Jenga from source requires:
- [haskell](https://www.haskell.org)
- [stack](https://docs.haskellstack.org/en/stable/)
- [cram](https://bitheap.org/cram/)  (Optional; for testing)

The easiest way to get `ghc` haskell and `stack` is via:
- [ghcup](https://www.haskell.org/ghcup/).


## Tutorial

Once you have `ghc` and `stack` installed, please proceed to the
[tutorial index](tutorial/index.md) to get `jenga` installed,
and learn how to setup and run jenga builds.
