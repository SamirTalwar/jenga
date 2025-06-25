# jenga2

Redo of the jenga build system.
Early days still. Figuring out the [Interface](src/Interface.hs)
between [UserMain](src/UserMain.hs) and the [Engine](src/Engine.hs).

## Demo build of small c-code [example](example).

Things to try:
- Build from empty cache: `rm -rf .cache; stack run -- -a`
- Flags to increase logging: `stack run -- -e -b -a -x`
- See the cache and artifacts: `find .cache ,jenga`
- Run built artifact: `,jenga/artifacts/example/main.exe`
- See the build witnesses: `cat .cache/traces/*`

- Make source changes while build watcher is running:
```
rm -rf .cache
watch -n 1 stack run -- -a
```

## Features

- Language Agnostic
- Haskell DSL for build rule construction.
- Monadic rule generation & dependencies.
- Hermetic (Sandboxed) builds.
- Minimal rebuilds via md5sum caching and constructive traces (Witnesses).
- Cloud builds possible via sharing `.cache/`.
- No builtin Watcher mode (yet!)

## TODO

Lots of examples!
