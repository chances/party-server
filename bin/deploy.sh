#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

cd $TRAVIS_BUILD_DIR

# Setup Heroku deployment tool
HEROKU_DEPLOY_BINARY_VERSION="v0.2.1.0"
HEROKU_DEPLOY_BINARY="heroku-deploy-binary-linux-x86_64"

echo "Downloading heroku-deploy-binary..."

mkdir dist/
curl -L \
  "https://github.com/joozek78/heroku-deploy-binary/releases/download/$HEROKU_DEPLOY_BINARY_VERSION/$HEROKU_DEPLOY_BINARY" \
  > ./dist/$HEROKU_DEPLOY_BINARY
chmod +x ./dist/$HEROKU_DEPLOY_BINARY

echo "Done"
echo ""

# Pull requests shouldn't try to deploy
if [ "$TRAVIS_PULL_REQUEST" != "false" ]; then
    echo "Skipping deploy on PRs"
    exit 0
fi

echo "Installing chances-party-exe to ./dist"
echo ""

# Install the chances-party executable for deployment
stack --local-bin-path dist/ install # TODO: Add optimization flags

echo "Done"
echo ""
echo "Deploying chances-party-exe to Heroku..."

cd dist
sh $HEROKU_DEPLOY_BINARY chances-party chances-party-exe -t $HEROKU_KEY

echo ""
echo "Successfully deployed!"
echo ""

echo "Cleaning up Heroku deployment tool"
rm $HEROKU_DEPLOY_BINARY
cd ..

echo ""
echo "Done"
echo ""
