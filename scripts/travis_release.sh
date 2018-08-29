#!/bin/bash

set -e

if [[ ! -z "$TRAVIS_TAG" ]] && [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  echo "Generating source distribution package and uploading it to Hackage..."
  mkdir -p "$HOME/.stack/upload"
  echo "{ \"username\": \"$HACKAGE_USERNAME\", \"password\": \"$HACKAGE_PASSWORD\" }" > "$HOME/.stack/upload/credentials.json"
  stack sdist
  stack upload .
else
  echo "Skipping Hackage release because this is not a tagged build."
fi