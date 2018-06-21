#!/bin/bash

set -e

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

# Download and unpack the stack executable
if [[ ! -x ~/.local/bin/stack ]]; then
  curl -sSL https://get.haskellstack.org/ | sh
fi
stack --version

# Install GHC
stack setup
stack exec -- ghc --version