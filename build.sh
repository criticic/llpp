#!/bin/sh
set -e
set -u

ghc -Wall -Werror --make Build.hs \
    -rtsopts -with-rtsopts=-I0

./Build "$@"
