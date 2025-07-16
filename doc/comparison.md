
# Comparison between Jenga with other build systems

Which of Jenga's features are present or missing in other build tools?\
[Make](https://en.wikipedia.org/wiki/Make_(software))
[Shake](https://shakebuild.com/)
[Bazel](https://bazel.build/)
[Buck](https://buck2.build/)
[Ninja](https://ninja-build.org/)
[Dune](https://dune.build/)
[Jenga-v1](history.md)
.

## General Purpose
Present: Make, Shake, Bazel, Buck, Ninja and Jenga-v1.\
Missing: Dune.

## Sandboxing
Present: Bazel, Buck and Dune.\
Missing: Make, Shake, Ninja, and Jenga-v1.

## Caching
Present: Bazel and Buck. (I am unsure if even Bazel and Buck provide _full history_ caching.)\
Missing: Make, Shake, Ninja, Dune and Jenga-v1.

## Powerful
Present: Shake, and Jenga-v1.\
Missing: Make, Bazel, Buck, Ninja and Dune.

## Simple
Present: Make, Ninja.\
Missing: Shake, Bazel, Buck, Dune and Jenga-v1.

Simplicity is of course somewhat in the eye of the beholder,
although I never met anyone who thought Bazel was simple.
Constructing build rules directly in a Haskell or Ocaml EDSL (Jenga, Shake, Jenga-v1) is nice,
but the simple _make-style_ syntax of Jenga is very familiar and more approachable, all the while retaining the power of the underlying Haskell EDSL.

As a bonus, Jenga can exist as standalone executable;
a Jenga operator does not require access to a Haskell development system.
