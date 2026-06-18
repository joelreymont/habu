#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

HB=${HB:-bin/hb}
[ -x "$HB" ] || { echo "map-test: hb missing"; exit 69; }

cat lib/errors.f lib/string.f lib/map.f lib/map-test.f | "$HB" | grep -F "map-test: ok" >/dev/null
