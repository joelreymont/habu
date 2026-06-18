#!/bin/sh
set -e
cd "$(dirname "$0")/.."

[ -x bin/hb ] || { echo "argv-test: bin/hb missing"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-argv.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

BUNDLE=$T/argv-test.f
cat lib/argv.f lib/argv-test.f > "$BUNDLE"

bin/hb "$BUNDLE" | grep -F "argv-test: ok" >/dev/null
bin/hb "$BUNDLE" --json -o OUT -- file.f --literal | grep -F "argv-test: ok" >/dev/null
./tools/check.sh lib/argv.f >/dev/null
