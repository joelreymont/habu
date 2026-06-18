#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}

fail() {
  echo "array-test: $*" >&2
  exit 1
}

[ -x "$HB" ] || fail "missing executable $HB"
[ -f lib/array.f ] || fail "missing lib/array.f"

cat lib/errors.f lib/array.f lib/array-test.f | "$HB"
