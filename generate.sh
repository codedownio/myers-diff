#!/usr/bin/env bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPTDIR

$(nix build .#cabal2nix --no-link --json | jq -r '.[0].outputs.out')/bin/cabal2nix . > default.nix
