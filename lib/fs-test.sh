#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

if [ -n "${HABU_HB:-}" ]; then
  HB=$HABU_HB
elif [ -x bin/hb ]; then
  HB=bin/hb
else
  HB=/Users/joel/Work/habu/bin/hb
fi

fail() {
  echo "fs-test: $*" >&2
  exit 1
}

[ -x "$HB" ] || fail "missing executable $HB"
[ -f lib/errors.f ] || fail "missing lib/errors.f"
[ -f lib/string.f ] || fail "missing lib/string.f"
[ -f lib/fs.f ] || fail "missing lib/fs.f"

cat lib/errors.f lib/string.f lib/fs.f lib/fs-test.f | "$HB"
