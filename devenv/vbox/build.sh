#!/usr/bin/env bash
set -e
set -x
cd $(dirname $0)/../../
if [ ! -e VERSION ]; then
  echo "Couldn't find version. Dying.!"
  exit 1;
fi
VERSION=$(cat VERSION)

rm -f Dockerfile_* result .ghc.environment*
echo -n "postgres://conduit:conduit@localhost/conduit" > config/backend/pgConnStr
echo -n "postgres://conduit:conduit@localhost/conduit" > reflex-realworld-example/config/backend/pgConnStr

VBoxManage unregistervm --delete "Reflex Realworld workshop (NixOS)" 2> /dev/null && true
nix-build devenv/vbox
VBoxManage import --vsys 0 --unit 5 --ignore result/reflex-realworld-workshop.ova
