#!/bin/bash

set -eu

git remote add travis "https://Hexirp:${TOKEN}@github.com/Hexirp/blog.git" &> /dev/null

git config --global user.name "Hexirp"
git config --global user.email "Hexirp@users.noreply.github.com"

git config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"

git checkout -b now-source

stack exec -- hexirp-blog-exe build

git add docs/
git commit -m "Build: by TravisCI (${TRAVIS_BUILD_NUMBER})"

git fetch origin master
git merge -s ours -m "Merge: by TravisCI (${TRAVIS_BUILD_NUMBER})" --no-ff origin/master

git checkout -b master origin/master
git merge now-source --ff-only

git push travis master &> /dev/null
