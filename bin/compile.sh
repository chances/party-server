#!/bin/bash

echo "Installing chances-party-exe to ./dist"

# Install the chances-party executable for deployment
stack --local-bin-path dist/ install # TODO: Add optimization flags?

cp Procfile dist/.

echo "Copying public assets"

cp -r public dist/.
