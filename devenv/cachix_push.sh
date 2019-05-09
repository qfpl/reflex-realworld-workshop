#!/usr/bin/env bash
set -e
set -x

cd $(dirname $0)/../
if [ ! -e VERSION ]; then
  echo "Couldn't find version. Dying.!"
  exit 1;
fi
VERSION=$(cat VERSION)

nix-build -A shells.ghc | cachix push qfpl
nix-store -q --requisites $(nix-instantiate -A shells.ghc) | grep cabal2nix | cachix push qfpl
