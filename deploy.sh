#!/bin/bash

# -e によりエラーになったら終了する
# -u により未定義の変数があったら終了する
set -eu

# TravisCI でプッシュするためのリモートを追加する
# トークンを URL に使っているので漏れないように /dev/null に送る
git remote add travis "https://Hexirp:${TOKEN}@github.com/Hexirp/blog.git" &> /dev/null

# ユーザーを Hexirp として設定する
git config --global user.name "Hexirp"
git config --global user.email "Hexirp@users.noreply.github.com"

# 謎のエラーを出さないため (https://github.com/Hexirp/blog/pull/5)
git config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"

# now-source ブランチで作業する
git checkout -b now-source

# ブログをコンパイルする
stack exec -- hexirp-blog-exe build

# docs/ フォルダをコミットして掃除する
git add docs/
git commit -m "Build: by TravisCI (${TRAVIS_BUILD_NUMBER})"
git stash

# master ブランチをフェッチしてチェックアウトする
# now-source ブランチをマージするコミットを作る
git fetch origin master
git checkout -b master origin/master
git merge -s ours -m "tmp" --no-ff now-source

# new-source ブランチをマージするコミットを残すために tmp ブランチを作る
git branch tmp

# master ブランチが参照するコミットを now-source ブランチのそれにセットする
# master ブランチの内容 (now-source の物と同じ) をステージングされた状態で
# 残しながら、master ブランチが参照するコミットを tmp ブランチのそれへセットする
git reset --hard now-source
git reset --soft tmp

# ステージングされているものをコミットする
git commit --amend -m "Merge: by TravisCI (${TRAVIS_BUILD_NUMBER})"

# ビルド結果をデプロイする (master ブランチへ作ったコミットをプッシュする)
# トークンを URL に使っているので漏れないように /dev/null に送る
git push travis master &> /dev/null
