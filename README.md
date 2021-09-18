## beam-t1

[beam](https://haskell-beam.github.io/beam/) and [SQLite](https://www.sqlitetutorial.net/) haskell prototype

## Description

A simple prototype for

- quickcheck and tasty framework
- sqlite
- beam
  Note:
  There are some issues with [nix](https://github.com/NixOS/nixpkgs) and beam. The purpose of this project was to come up to speed with haskell, and beam. For now I am ignoring the nix - beam issue.

## build & test

To

- build the project

```
stack build
```

- execute the project

```
stack run
```

- executing the test continuesly

```
stack test --file-watch
```
