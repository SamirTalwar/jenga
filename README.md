# jenga2

Redo of the jenga build system.
Early days still. Figuring out the [Interface](src/Interface.hs)
between [UserMain](src/UserMain.hs) and the [Engine](src/Engine.hs).

## Prerequisites

- [haskell](https://www.haskell.org) : jenga2 is written and configured using haskell.
- [cram](https://bitheap.org/cram/) : for testing jenga2.

## Build and test

`stack build; cram test`

## Features

- Language Agnostic.
- Haskell DSL for build rule construction.
- Monadic rule generation & dependencies.
- Safe, hermetic builds via sandboxing.
- Minimal rebuilds via constructive traces.
- Early build cutoff when outputs are unchanged.
- Full caching; no rebuild required to return to previous build state.
- Cloud builds possible via sharing `.cache/`.
- No builtin Watcher mode (yet!)

## Explore build of small c-code [example](example).

Things to try:

- Build from empty cache: `rm -rf .cache; stack run -- -a`
- Flags to increase logging: `stack run -- -v -x -i`
- See the cache and artifacts: `find .cache ,jenga`
- Run built artifact: `,jenga/artifacts/example/main.exe`
- See the build witnesses: `cat .cache/traces/*`
- Keep and view the build sandboxes: `rm -rf .cache; stack run -- -k; find ,jenga/box`

- Make source code changes while build watcher is running:
```
rm -rf .cache
watch -n 1 stack run -- -a
```

## TODO

Lots more examples and tests.
