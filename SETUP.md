# Workshop Setup

This workshop is built as an [obelisk](https://github.com/obsidiansystems/obelisk/) application, so getting it runnable involves getting the obelisk command and running it so that you have all of the project dependencies. Whichever option you pick, please make sure to run all the steps so that you have no unexpected need to download things in the workshop.

This uses [nix](https://nixos.org/nix/) so you will need nix first. If you don't already have this setup and working, then it will be best to use the VM or docker images that have everything already installed for you. Nix also doesn't fully work on windows yet, but it [is very close](https://github.com/reflex-frp/reflex-platform/issues/303).

There's also a postgres database to setup. The VM and docker compose setup have this running already. You can use the docker database standalone in Nix/NixOS.

The VM is the easiest option as it has most things pre-installed, running and ready to go. The only thing you may want to do is configure a text editor (it has vim, spacemacs and vscode preinstalled).

The docker route should use less resources since it doesn't have to run a whole VM and allows you to use your editor on your machine. The docker way has some mild annoyances (writing files owned by root to the checkout and the haddocks aren't as accessible). The VM is probably the best route unless VBox doesn't work for you.

The Nix/NixOS route is the easiest if you are comfortable with nix. It has been mostly tested on Linux, but obelisk is supported on MacOS and at least one person has successfully built the workshop on MacOS. So it should be fine.

The VM and the Docker image is around 3GiB to download. With nix, it's about 700MiB of dependencies to download from the caches.

If any of these steps don't work for you, please either file a github issue, [![Chat on Gitter](https://badges.gitter.im/reflex-realworld-workshop/community.svg)](https://gitter.im/reflex-realworld-workshop/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) or jump onto #qfpl on irc.freenode.net.


Ben will have some USB drives with the VM and the docker layers on them if you're stuck on the day.

## VM

### Download OVA (Internet Required)

Download the OVA from running:

`curl https://s3-ap-southeast-2.amazonaws.com/reflex-realworld-workshop/reflex-realworld-workshop.$(curl https://s3-ap-southeast-2.amazonaws.com/reflex-realworld-workshop/OVA_LATEST_VERSION).ova > workshop.ova`.

If you don't have curl, just hit https://s3-ap-southeast-2.amazonaws.com/reflex-realworld-workshop/OVA_LATEST_VERSION in a browser and take that number and download https://s3-ap-southeast-2.amazonaws.com/reflex-realworld-workshop/reflex-realworld-workshop.$VERSION.ova.

### Import Appliance

Import that appliance by opening up the virtual box gui > File > Import Appliance > select file. The defaults should be fine.

### Test Setup

Boot that VM up. Once it is up, open up three terminals. These will download some final things, so it's important
to do this before the workshop.

  - `cd reflex-realworld-workshop` then `ob run`
  - `cd reflex-realworld-workshop/reflex-workshop-example` then `ob run`
  - `cd reflex-realworld-workshop` then `devenv/nix/hoogle-server`

You should be able to open three tabs up:
  - http://localhost:8000 - Fully working example
  - http://localhost:8001 - Workshop code
  - http://localhost:8080 - Local Hoogle Server

Once you have this setup, you should just be able to `git pull && git submodule update` and rerun both ob runs on the night before lamdbajam. This will ensure you have the latest code and there should be no huge redownloads or needing to
download the whole image again.

Please watch the repo for releases and/or join gitter to keep in touch or ask questions! :)

## Docker

### Checkout (Internet Required)

Checkout this code if you haven't already:

`git clone --recurse-submodules https://github.com/qfpl/reflex-realworld-workshop.git`

### Install Docker Compose (Internet Required)

You will need [docker compose](https://docs.docker.com/compose/install/) installed.

### Download Layers & Test (Internet Required)

Run this command: `devenv/docker/ob run-example` and load up http://localhost:8000 . If you see a page loaded with some articles, everything is all good.

Run this command `devenv/docker/ob run-workshop` and load up http://localhost:8001 . If you see a much blanker page with no errors, you're pretty good!

### Getting help / keeping up to date

Once you have this setup, you should just be able to `git pull && git submodule update` and rerun both ob runs on the night before lamdbajam. If I've had to change the image at all, this will download another ~3GiB at this time. Sorry! You'll have to reset the config files back so that the pull can happen.

Please watch the repo for releases and/or join gitter to keep in touch or ask questions! :)

## Nix / NixOS

### Checkout (Internet Required)

Checkout this code if you haven't already:

`git clone --recurse-submodules https://github.com/qfpl/reflex-realworld-workshop.git`

### Cache Setup

Add the nixcache.reflex-frp.org, qfpl.cachix.org and hydra.qfpl.io caches to your nix.conf or nixos configuration.

See [nix.conf](devenv/common/nix.conf) for a nix example or see a [NixOS](devenv/vbox/default.nix) example.

### Install Obelisk (Internet Required)

Follow the [obelisk installation instructions](https://github.com/obsidiansystems/obelisk/#installing-obelisk) ignoring the cache part because you already set that up in the last step.

### Setup the database (Internet Required)

The easiest way is to use docker. `docker run -p 5432:5432 benkolera/reflex-realworld-workshop-pg:latest` will give you a running database on localhost with the schema, data and credentials all good. This way is easiest if you already have docker. If you are already running postgres, you'll get an error so change the port to `-p 5433:5432`. Just be sure to change config/backend/pgConnStr to have a postgres:/conduit:conduit@localhost:5433/conduit in both the workshop and examples to reflect the port change.

If you don't have docker and would rather set things up in postgres yourself what we need is:
  - A user called conduit that can access the database over tcp with a username and password.
  - A database called conduit, preferably owned by conduit so you don't need to do grants. devenv/vbox/setup.sql creates the DB and user with a default password of conduit.
  - The database tables and table data. The dump in devenv/common/dbdump.sql. creates the DB and schema and test data.
  - If you change any details, be sure to reflect them in the config/backend/pgConnStr config in the workshop and full example.

### Run the workshop (Internet Required)

Everything should be cached on qfpl.cachix.org. If you've setup the caches it should just be some downloading (of about 750MiB) rather than building a lot of code from source. Chris McKay has built it on MacOS nix and it worked fine and should now be cached thanks to him too. :)

Run `ob run` in: `./reflex-realworld-example` and then check out http://localhost:8000 . You should see a page with some articles and ponies on it. If this works, the DB is all setup and the code is built.

Repeat `ob run` in this checkout and check out http://localhost:8001 . This should be more blank, but it should load a page with the nav bar and title.

If this is all happy you are good to go! You may need to git pull before the workshop again, but that'll be a much less painful experience.

### Getting help / keeping up to date

Once you have this setup, you should just be able to `git pull && git submodule update` and rerun both ob runs on the night before lamdbajam.

Please watch the repo for releases and/or join gitter to keep in touch or ask questions! :)
