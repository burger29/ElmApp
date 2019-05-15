#!/usr/bin/env sh
set -o errexit -o xtrace
watchy \
  --watch 'elm-package.json' \
  --watch 'src/**/*.elm' \
  -- "$@"
