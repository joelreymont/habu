#!/bin/sh
# check.sh — feed a program to the checked native engine. `--json-errors`
# switches reject diagnostics from prose to structured JSON before user code runs.
set -e
cd "$(dirname "$0")/.."
JSON=0
if [ "${1:-}" = "--json-errors" ]; then
  JSON=1
  shift
fi
[ "$#" -le 1 ] || { echo "usage: tools/check.sh [--json-errors] [prog.f]"; exit 64; }
[ -x bin/habu ] || { echo "check.sh: bin/habu missing (run tools/snap-hb.sh first)"; exit 69; }
T=$(mktemp "${TMPDIR:-/tmp}/habu-check.XXXXXX")
cleanup() { rm -f "$T"; }
trap cleanup EXIT HUP INT TERM
if [ "$JSON" = 1 ]; then
  printf '%s\n' '-1 JSON-DIAGS !' > "$T"
else
  : > "$T"
fi
if [ "$#" = 1 ]; then
  [ -f "$1" ] || { echo "check.sh: no such source: $1"; exit 66; }
  cat "$1" >> "$T"
else
  cat >> "$T"
fi
bin/habu < "$T"
