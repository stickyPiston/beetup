# Beetup Backend

## Installing

Make sure you use `GHC 4.17.2.1`, and then do:

```bash
cabal update && cabal build
```

## Usage

```bash
cabal run
```

Don't forget to run the frontend as well if you would like to use the backend as
a sane person.

## Tests

In order to run the tests, run the following command in this `backend` folder:

```bash
$ cabal test --enable-tests
Resolving dependencies...
```