# isl-hs

In-progress Haskell bindings to the integer set library.

<img src="https://travis-ci.org/joelburget/isl-hs.svg?branch=master" />

## Building the package

* Run `nix-build release.nix`

## Development

* Enter a development environment: `nix-shell`

Then:

* Start a REPL: `cabal repl lib:isl-hs`
* Run ghcid: `ghcid --command "cabal repl lib:isl-hs"`
