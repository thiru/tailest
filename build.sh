#!/bin/bash

if [ $# = 0 ] || [ $1 = "sbcl" ]; then
  echo "Building executable using SBCL..."
  echo ""
  sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options \
       --noprint --disable-debugger --load build.lisp
  echo ""
  echo "Finished building executable using SBCL"
elif [ $1 = "ccl" ]; then
  echo "Building executable using CCL..."
  echo ""
  ccl --load build.lisp
  echo ""
  echo "Finished building executable using CCL"
else
  echo "Sorry, only SBCL and CCL are currently supported."
fi

# We touch this file so that the next run of the app will use it rather
# than the app binary we just created
touch LICENSE
