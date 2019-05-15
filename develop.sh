#!/usr/bin/env sh
set -o errexit -o xtrace

 rm -rf elm-stuff

 exec docker-compose up --build --force-recreate --remove-orphans
