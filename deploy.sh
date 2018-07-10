#!/bin/bash

set -eu

NAME=Hexirp
EMAIL=Hexirp@users.noreply.github.com

git remote add travis "https://${TOKEN}@github.com/Hexirp/blog.git"

git config --global user.name "${NAME}"
git config --global user.email "${EMAIL}"

stack exec -- hexirp-blog-exe build

git add docs/
git commit -m "Build: by TravisCI (${TRAVIS_BUILD_NUMBER})"
git checkout -b now-source

git pull origin master
git merge -s ours -m "Merge: by TravisCI (${TRAVIS_BUILD_NUMBER})" --no-ff master

git checkout master
git merge now-source --ff-only

git push travis master
