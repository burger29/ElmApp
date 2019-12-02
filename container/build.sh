#!/usr/bin/env sh
set -o errexit -o xtrace
elm make src/Main.elm --debug --output=output/elm-app.js
node-sass styles/gradestyles.scss --output-style=compressed --output=dist
