#!/usr/bin/env bash
[ -f "$NIX_STORE/unsafe-env" ] && source "$NIX_STORE/unsafe-env"
exec bash "$@"
