#!/bin/sh
# bench.sh — fixed kernels on bin/hb; informs the regalloc tier decisions
# (pool width vs locals cache vs float pool). Prints ns/iter; gates nothing.
# Float kernels join when the d-reg pool work starts (dot caf-1002e9de).
cd "$(dirname "$0")/.."
[ -x bin/hb ] || { echo "no bin/hb — run tools/build.sh"; exit 1; }
exec bin/hb tools/bench.f "$@"
