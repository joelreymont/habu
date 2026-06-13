#!/bin/sh
# hb-aot.sh — alias for the default (AOT) mode of hb-build.sh, kept for clarity.
# `hb-build.sh prog.f -o out` already defaults to AOT (engine stripped).
exec "$(dirname "$0")/hb-build.sh" "$@"
