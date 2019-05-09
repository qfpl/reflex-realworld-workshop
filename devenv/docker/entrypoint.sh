#!/usr/bin/env bash
set -e
if [ $(id -u) != 0 ]; then
    echo "You must run this docker container as root (i.e without the --user flag)" >&2;
    exit 1;
fi;
if [ ! -d /workshop ]; then
    echo "Please run this with the workshop code as a volume at /workshop. Eg. docker run -ti -p 8000:8000 -v $(pwd):/workshop -w /workshop benkolera/reflex-realworld-workshop:latest $@" >&2
    exit 1;
fi;

# Lets cheat a little, and just create users and groups for the uids/gids that
# own the volume.
WORK_UID=$(stat -c '%u' /workshop);
WORK_GID=$(stat -c '%g' /workshop);

if getent group $WORK_GID; then
    WORK_GROUP=$(getent group $WORK_GID | cut -f1 -d':');
else
    WORK_GROUP="workshop";
    addgroup -g $WORK_GID $WORK_GROUP;
fi

if getent passwd $WORK_UID; then
  WORK_USER=$(getent passwd $WORK_UID | cut -f1 -d':');
else
  WORK_USER="workshop";
  adduser -D -u $WORK_UID $WORK_USER $WORK_GROUP;
fi

su-exec $WORK_USER:$WORK_GROUP $@
