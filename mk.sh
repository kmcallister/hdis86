#!/bin/sh

cabal configure --extra-lib-dirs=$HOME/local/lib --extra-include-dirs=$HOME/local/include -p "$@" && cabal build
