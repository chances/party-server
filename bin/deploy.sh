#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

# Setup Heroku binary deployment tool
HEROKU_DEPLOY_BINARY_VERSION="v0.2.1.0"
HEROKU_DEPLOY_BINARY="heroku-deploy-binary-linux-x86_64"

mkdir dist/
travis_retry curl -L \
https://github.com/joozek78/heroku-deploy-binary/releases/download/$HEROKU_DEPLOY_BINARY_VERSION/$HEROKU_DEPLOY_BINARY
mv $HEROKU_DEPLOY_BINARY dist/$HEROKU_DEPLOY_BINARY

# Pull requests shouldn't try to deploy
if [ "$TRAVIS_PULL_REQUEST" != "false" ]; then
    echo "Skipping deploy on PRs"
    exit 0
fi

# Save some useful information
REPO=`git config remote.origin.url`
SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
SHA=`git rev-parse --verify HEAD`

# Clean out existing contents
rm -rf out/**/* || exit 0

doInstall

# Install the chances-party executable for deployment
stack --local-bin-path dist/ install # TODO: Add optimization flags
cd dist
sh $HEROKU_DEPLOY_BINARY chances-party chances-party-exe -t $HEROKU_KEY
