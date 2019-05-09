#!/usr/bin/env bash

set -e
set -x
if [ ! -e VERSION ]; then
  echo "Run from the project root, dummy!"
fi
VERSION=$(cat VERSION)
TAG_PREFIX="benkolera/reflex-realworld-workshop"
PG_TAG_PREFIX="$TAG_PREFIX-pg"
OB_TAG_PREFIX="$TAG_PREFIX"
OUT_DIR=~/.cache/reflex-realworld-workshop/
OUT_PG_NAME=reflex-realworld-workshop-docker-db.$VERSION.tar
OUT_OB_NAME=reflex-realworld-workshop-docker.$VERSION.tar

ln -f devenv/docker/Dockerfile_obelisk
ln -f devenv/docker/Dockerfile_db
mkdir -p $OUT_DIR
rm -fr .ghc.environment* result

docker login
docker build . -f Dockerfile_db -t $PG_TAG_PREFIX:$VERSION
docker tag $PG_TAG_PREFIX:$VERSION $PG_TAG_PREFIX:latest
docker build . -f Dockerfile_obelisk -t $OB_TAG_PREFIX:$VERSION
docker tag $OB_TAG_PREFIX:$VERSION $OB_TAG_PREFIX:latest
docker push $PG_TAG_PREFIX:$VERSION
docker push $OB_TAG_PREFIX:$VERSION
docker push $PG_TAG_PREFIX:latest
docker push $OB_TAG_PREFIX:latest
docker save $PG_TAG_PREFIX | gzip -c > $OUT_DIR/$OUT_PG_NAME.gz
docker save $OB_TAG_PREFIX | gzip -c > $OUT_DIR/$OUT_OB_NAME.gz
