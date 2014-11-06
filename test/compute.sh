#!/usr/bin/env bash

#gtimeout 5 bash -c 'while true; do head -n 100 /dev/urandom; sleep .1; done | hexdump -C | grep --line-buffered "ca fe"'
gtimeout 5 bash -c 'while true; do head -n 100 /dev/urandom; sleep .1; done | hexdump -C | grep "ca fe"; echo'
