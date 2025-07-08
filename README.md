# jenga2

Skip the why, and jump directly to [getting started](tutorial/01_getting_started.md).

This project is a redo of the `jenga` build system, which I first wrote whilst working for Jane Street in around 2010. Original `jenga` continued to be used within Jane Street past my leaving in 2016 and right up until 2024, as discussed in this
[Jane Street Blog](https://blog.janestreet.com/how-we-accidentally-built-a-better-build-system-for-ocaml-index/)

Jane Street has now switched to using `dune` internally. I've used `dune` and I really like it. It is one of the best build systems out there, although it is biased towards mainly building `ocaml`, whereas `jenga` (original and this redo) are general purpose build systems with no hard-coded built in support for any specific language (following the tradition of build systems like `make`, `shake`, `bazel` and `buck`).

Recently, I got nerd-sniped into reimplementing `jenga` after discussion with friends.
I was pretty proud of original jenga; I really like the concept of a general purpose build system, and I thought I could make something even better. In particular, features that original jenga lacked -- proper sandboxing and full shared caching -- I now regard as very important. This redo is designed with support for these features from the outset. Other features that original jenga supported remain part of jenga2, specifically: minimal rebuilds, dynamic dependencies and dynamic rule construction.

This redo of jenga shares no code with the original;
I have not looked at the code of original jenga since I left Jane Street in 2016.
It does however share my experience and opinions of what features are important for a good build system.
Original jenga was written in `ocaml`. This redo is written in `haskell`.
Since original `jenga` no longer exists, even within Jane Street, I will refer to the new version simply as `jenga`.


## Prerequisites

- [haskell](https://www.haskell.org) : jenga is written and configured using haskell.
- [cram](https://bitheap.org/cram/) : for testing jenga. (Optional).

Currently `jenga` is setup to be built using
[`stack`](https://docs.haskellstack.org/en/stable/)
which is available as part of the standard [haskell tooling](https://www.haskell.org/downloads/)
The easiest way to get `ghc` haskell and `stack` is via [ghcup](https://www.haskell.org/ghcup/).


## Getting started

Once you have `ghc` and `stack` installed, please proceed to the
[tutorial index](tutorial/index.md) to get `jenga` installed,
and learn how to run jenga builds.

You might also like to browse the [source code](src) and [tests](test) for jenga.

The tutorial gives examples of using `build.jenga` files to configure Jenga.
[The syntax is specified here](doc/jenga-syntax.md).
