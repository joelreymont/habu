#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}

fail() {
  echo "property-test: $*" >&2
  exit 1
}

[ -x "$HB" ] || fail "missing executable $HB"

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-property.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

out=$("$HB" --load lib/errors.f lib/test.f lib/property.f lib/property-test.f)
[ "$out" = "test: ok" ] || fail "unexpected output: $out"

CHECK=$T/property-check.f
cat lib/errors.f lib/property.f > "$CHECK"
./tools/check.sh "$CHECK" >/dev/null

echo "property-test: ok"
