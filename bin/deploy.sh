#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

SOURCE_BRANCH=`git rev-parse --abbrev-ref HEAD`
TARGET_BRANCH="staging"

function doCompile {
  ./bin/compile.sh
}

# Pull requests and commits to other branches shouldn't try to deploy, just build to verify
if [ "$TRAVIS_PULL_REQUEST" != "false" ]; then
    echo "Skipping deploy in PR; just doing a build."
    doCompile
    exit 0
fi

# Save some useful information
REPO=`git config remote.origin.url`
SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
SHA=`git rev-parse --verify HEAD`

# Add deploy key to ssh agent to push to GitHub
openssl aes-256-cbc -K $encrypted_5894ebfdb1ab_key -iv $encrypted_5894ebfdb1ab_iv \
  -in deploy_key.enc -out deploy_key -d
chmod 600 deploy_key
eval `ssh-agent -s`
ssh-add deploy_key

# Clone the existing gh-pages for this repo into dist/
# Create a new empty branch if gh-pages doesn't exist yet (should only happen on first deploy)
git clone $REPO dist
cd dist
git checkout $TARGET_BRANCH || git checkout --orphan $TARGET_BRANCH
cd ..

# Clean existing contents from dist
rm -rf dist/**/* || exit 0

# Run our compile script
doCompile

# Now let's go have some fun with the cloned repo
cd dist
git config user.name "Travis CI"
git config user.email "git@chancesnow.me"

# If there are no changes to the compiled dist (e.g. this is a README update) then just bail.
if [ -z `git diff --exit-code` ]; then
    echo "No changes to the output on this push; exiting."
    exit 0
fi

# Commit the "changes", i.e. the new version.
# The delta will show diffs between new and old versions.
git add .
git commit -m "Deploy to staging: ${SHA}"

# Now that we're all set up, we can push.
git push $SSH_REPO $TARGET_BRANCH
