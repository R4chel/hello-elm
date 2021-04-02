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

# Build 
echo "building site..."
cp public/index.html $outdir/index.html
elm make ./src/Main.elm --optimize --output=./$outdir/elm.js

# TODO get this to work
# uglifyjs ./$outdir/elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=./$outdir/elm.min.js


# Add & Commit & Deploy to GH Pages
git add .
git commit -m "rebuild triggered - `date`"
git subtree push --prefix $outdir origin gh-pages

echo "Done"
