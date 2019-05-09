#!/usr/bin/env bash
set -e
set -x

cd $(dirname $0)/../../
if [ ! -e VERSION ]; then
  echo "Couldn't find version. Dying.!"
  exit 1;
fi
VERSION=$(cat VERSION)
RELEASE=1

OUT_DIR=~/.cache/reflex-realworld-workshop/
mkdir -p $OUT_DIR
OVA_VERSION="$VERSION.$RELEASE"
OVA_NAME=reflex-realworld-workshop.$OVA_VERSION.ova
OVA=$OUT_DIR/$OVA_NAME

rm -f $OVA
VBoxManage export "Reflex Realworld workshop (NixOS)" -o $OVA
aws s3 cp $OVA s3://reflex-realworld-workshop/$OVA_NAME
echo $OVA_VERSION > $OUT_DIR/OVA_LATEST_VERSION
aws s3 cp $OUT_DIR/OVA_LATEST_VERSION s3://reflex-realworld-workshop/OVA_LATEST_VERSION
