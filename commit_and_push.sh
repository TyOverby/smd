#!/usr/bin/env bash 

rm -rf ./docs
mkdir docs
cp _build/default/bindgen/bin/*.{html,css,js} ./docs

git add -A
git commit -m _
git push origin master
