# Changelog
All changes to the workshop will be documented here with instructions for how you should update local code / vms when upgrading.

For each new version, there will also be a github release: so please watch this repo for new releases if you are keen to be notified! :)

## Version 0.1.0.1 - 2019-05-13-02:00
Just finalised some code changes to example that will get us to the end of the workshop. 

Adds tag loading to the home page and selecting feed/global.

Adds some test data in that makes it a bit easier to test the feed and tags.

Workshop instructions should come this evening. Just tidying it all off still. :)

### Upgrade instructions

- If you are on the VM and have already downloaded it, please:
  - Do a git pull in the ~/reflex-realworld-workshop directory
  - To update the DB to the latest schema: `psql -h localhost -U conduit -c "DROP SCHEMA PUBLIC CASCADE; CREATE SCHEMA PUBLIC;" && psql -h localhost -U conduit conduit -f ~/reflex-realworld-workshop/devenv/common/dbdump.sql`
  - `ob run` in both ~/reflex-realworld-workshop and ~/reflex-realworld-workshop/reflex-realword-example` 
  - If the update worked, on http://localhost:8000 you should see tags in the list of "being cool" and "parties" and clicking on them should filter the list down to a single article.
- If you are using to docker setup):
  - git pull in your local checkout
  - devenv/docker/ob stop
  - `docker pull benkolera/reflex-realworld-workshop-pg:0.1.0`
  - devenv/docker/ob run-example
  - If the update worked, on http://localhost:8000 you should see tags in the list of "being cool" and "parties" and clicking on them should filter the list down to a single article.
- If you are using nix/NixOS:
  - git pull in your checkout
  - If you are using the docker database: 
    - devenv/docker/ob stop
    - `docker pull benkolera/reflex-realworld-workshop-pg:0.1.0`
    - Run the database again
  - If you have a local postgres DB:
    - `psql -h localhost -U conduit -c "DROP SCHEMA PUBLIC CASCADE; CREATE SCHEMA PUBLIC;" && psql -h localhost -U conduit conduit -f ~/reflex-realworld-workshop/devenv/common/dbdump.sql`
  - ob run in the example subfolder.
  - If the update worked, on http://localhost:8000 you should see tags in the list of "being cool" and "parties" and clicking on them should filter the list down to a single article.

## Version 0.1.0 - 2019-05-08
This is the first release without any content. This is the skeleton of the code and all the dependencies. If you follow the [setup instruction](./SETUP.md) to a clean ob run, then it should just be a git pull later to get the exercises and it should just work without any big rebuilds or waits.

- Released initial version
