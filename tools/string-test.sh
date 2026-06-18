#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}

fail() {
  echo "string-test: $*" >&2
  exit 1
}

[ -x "$HB" ] || fail "missing executable $HB"
[ -f lib/string.f ] || fail "missing lib/string.f"

cat lib/string.f lib/string-test.f | "$HB"
