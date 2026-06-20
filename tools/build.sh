#!/bin/sh
# build.sh — rebuild the single installed binary, bin/hb, USING bin/hb.
#
# bin/hb is the checked native engine users run. Build-only compiler engines are
# temporary files under $HB_TMP. The checked Habu driver owns the source assembly,
# fixpoint loop, byte comparison, process execution, filesystem mutation, signing
# commands, and artifact expectations. This shell wrapper owns private temp setup
# and final installation of the already validated hb-new artifact.
set -e
cd "$(dirname "$0")/.."
CLEAN_T=0
if [ -n "${HB_TMP:-}" ]; then
  T=$HB_TMP
else
  T=$(mktemp -d "${TMPDIR:-/tmp}/hb-rebuild.XXXXXX")
  CLEAN_T=1
fi
mkdir -p "$T"
export HB_TMP=$T
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM
[ -x bin/hb ] || { echo "no bin/hb — install a trusted seed with tools/seed.sh /path/to/hb"; exit 1; }

HB_TMP=$T bin/hb --load \
  lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/build.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f

test -f "$T/hb-new" || { echo "build: checked hb image not produced"; exit 1; }
/bin/mv "$T/hb-new" bin/hb
/usr/bin/find bin -maxdepth 1 -type f ! -name hb -delete
echo "build OK: bin/hb (checked engine, tty REPL + stdin)"
