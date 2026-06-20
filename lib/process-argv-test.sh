#!/bin/sh
set -e
cd "$(dirname "$0")/.."

if [ -n "${HABU_HB:-}" ]; then
  HB=$HABU_HB
elif [ -x bin/hb ]; then
  HB=bin/hb
else
  HB=/Users/joel/Work/habu/bin/hb
fi

[ -x "$HB" ] || { echo "process-argv-test: bin/hb missing"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-process-argv.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

LONG_ARG=$(printf '%32769s' '' | tr ' ' a)
"$HB" --load lib/errors.f lib/test.f lib/process.f lib/process-argv.f \
  lib/process-argv-test.f -- "$LONG_ARG" | grep -F "process-argv-test: ok" >/dev/null
cat lib/errors.f lib/process.f lib/process-argv.f | ./tools/check.sh >/dev/null
