#!/usr/bin/env bash
cd $(dirname $0)
set -ex

echo "Started deploying"

outdir=dist
rm -rf $outdir || exit 0;
mkdir -p $outdir


cd $(dirname "$0") || exit

if [ -n "$(git status --porcelain)" ]; then
	  echo "working directory not clean, exiting." 
	  exit 1
fi

if ! git diff --exit-code > /dev/null; then
	  echo "working directory not clean, exiting." 
	  exit 1
fi 
git checkout -b gh-pages &> /dev/null
trap "git checkout - &> /dev/null" EXIT

# Build 
echo "building site..."
cp public/index.html $outdir/index.html
elm make ./src/Main.elm --optimize --output=./$outdir/elm.js

# TODO get this to work
# uglifyjs ./$outdir/elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=./$outdir/elm.min.js


#TODO use $outdir not 'dist'
# Delete and move files.
find . -maxdepth 1 ! -name 'dist' ! -name '.git' ! -name '.gitignore'  -exec rm -rf {} \;
mv dist/* .
rm -R dist/

# Push to gh-pages.
echo "committing compiled site..."
git add -A > /dev/null
git commit --allow-empty -m "$(git log -1 --pretty=%B)" > /dev/null
echo "pushing compiled site..."
git push -f -q origin gh-pages > /dev/null


echo "Deployed Successfully! <3"

exit 0
