#!/bin/sh

COMMIT=$(git -C $DUNE_SOURCEROOT describe --always --dirty --abbrev=7)

BUILD_DATE=$(date '+%Y-%m-%d')

sed -e "s/%%VERSION%%/$COMMIT/g" \
    -e "s/%%BUILD_DATE%%/$BUILD_DATE/g" \
    $1
