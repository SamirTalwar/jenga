
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

- __General Purpose__: Jenga, Make, Shake, Bazel, Buck, Ninja and Jenga-v1.\
_Missing_: Dune.

- __Sandboxing__: Jenga, Bazel, Buck and Dune.\
_Missing_: Make, Shake, Ninja, and Jenga-v1.

- __Caching__: Jenga, Bazel and Buck.\
_Missing_: Make, Shake, Ninja, Dune and Jenga-v1.

- __Powerful__: Jenga, Shake, and Jenga-v1.\
_Missing_: Make, Bazel, Buck, Ninja and Dune.

Neil Mitchell (author of Shake and major contributor to Buck2) describes monadic dependencies as ["the most important thing shake got right"](https://neilmitchell.blogspot.com/2021/09/reflecting-on-shake-build-system.html).

- __Simple__: Jenga, Make, Ninja.\
_Missing_: Shake, Bazel, Buck, Dune and Jenga-v1.

Simplicity is of course somewhat in the eye of the beholder,
although I never met anyone who thought Bazel was simple.
Constructing build rules directly in a Haskell or Ocaml EDSL is nice,
but I believe the simple _make-style_ syntax of Jenga is more approachable.
