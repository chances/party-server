#!/bin/bash

echo "Installing chances-party-exe to ./dist"

# Install the chances-party executable for deployment
# TODO: Add optimization flags? (--ghc-options=-O)
stack install --local-bin-path ./

cp chances-party-exe Procfile dist/.

echo "Copying public assets"

cp -r public dist/.
