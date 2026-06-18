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

[ -x "$HB" ] || { echo "build-fixpoint-test: bin/hb missing"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-build-fixpoint.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

./tools/bundle-lib.sh -o "$T/build-fixpoint.f" \
  errors string fs fs-mutate process process-argv build -- tools/build-fixpoint.f

HB_TMP=$T "$HB" "$T/build-fixpoint.f" > "$T/build-fixpoint.out" 2> "$T/build-fixpoint.err"
grep -F "build OK: stage compiler fixpoint" "$T/build-fixpoint.out" >/dev/null
grep -F "build OK: hb-new validated" "$T/build-fixpoint.out" >/dev/null
test -f "$T/hb-new"
if find "$T" -maxdepth 1 -type f -perm -111 -name 'build-*' | grep . >/dev/null; then
  echo "build-fixpoint-test: found stale executable build shim" >&2
  exit 1
fi
grep -F ": HOOK CHECK ; ' HOOK set-check" "$T/stage2-src" >/dev/null
grep -F "STDIN-OUT" "$T/stage2-src" >/dev/null
grep -F "SNAP-MAGIC" "$T/hb-snap-src" >/dev/null

sed '$d' tools/build-fixpoint.f > "$T/build-fixpoint-defs.f"
cat lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f \
  lib/process-argv.f lib/build.f "$T/build-fixpoint-defs.f" |
  ./tools/check.sh >/dev/null

echo "PASS: build fixpoint driver"
