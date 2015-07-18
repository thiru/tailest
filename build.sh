#!/bin/bash

sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options \
     --noprint --disable-debugger --load build.lisp

# We touch this file so that the next run of the app will use it rather
# than the app binary we just created
touch LICENSE
