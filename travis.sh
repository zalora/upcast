#!/usr/bin/env bash

upcast="upcast-$TRAVIS_BUILD_NUMBER"

~/bin/stack build
~/bin/stack install
cp "$(~/bin/stack path | awk -F: '/^local-install-root/{print $2}')bin/upcast" $upcast
strip -p --strip-unneeded --remove-section=.comment -o $upcast
upx $upcast
