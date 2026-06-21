#!/bin/sh
# run.sh — the DEFAULT gate: habu-native, no gforth anywhere on the path.
#   lints -> self-rebuild fixpoint -> engine suite -> checked hb -> tty REPL ->
#   hb-build standalone.
set -e
cd "$(dirname "$0")/.."
if [ "${1:-}" = "full" ]; then
  echo "FAIL: test/run.sh full retired; the native gate is test/run.sh"
  exit 64
fi
if [ "$#" -gt 0 ]; then
  echo "usage: test/run.sh"
  exit 64
fi
CLEAN_T=0
if [ -n "${HB_TMP:-}" ]; then
  T=$HB_TMP
else
  T=$(mktemp -d "${TMPDIR:-/tmp}/hb-gate.XXXXXX")
  CLEAN_T=1
fi
mkdir -p "$T"
export HB_TMP=$T
CHECK="bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f tools/argv.f tools/check.f --"
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-stdlib.f || { echo "FAIL: native lint/stdlib gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f lib/build.f tools/build-fixpoint.f test/gate-engine.f || { echo "FAIL: native engine gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f test/gate-dictionary.f || { echo "FAIL: native dictionary/checker gate phase"; exit 1; }
[ -x bin/hb ] || { echo "FAIL: bin/hb not produced"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f test/gate-diagnostics.f || { echo "FAIL: native checker diagnostics gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f lib/codesign.f test/gate-debug.f || { echo "FAIL: native prop/snapshot/debug gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f test/gate-build-common.f test/gate-aot-positive.f || { echo "FAIL: native hb-build AOT positive gate phase"; exit 1; }
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f test/gate-build-common.f test/gate-aot-negative.f || { echo "FAIL: native hb-build AOT negative gate phase"; exit 1; }
# hb-build --repl = build-time checked user source + engine/REPL bundle.
printf ': SQ ( i64 -- i64 ) DUP * ;\nEXPORT SQ\n9 SQ . CR\n' > $T/hb-rt.f
./tools/hb-build.sh --repl $T/hb-rt.f -o $T/hb-rt >/dev/null || { echo "FAIL: hb-build --repl"; exit 1; }
[ "$($T/hb-rt)" = "81" ] || { echo "FAIL: hb-build --repl output (got: $($T/hb-rt))"; exit 1; }
bin/hb tools/imgdump.f $T/hb-rt > $T/hb-rt-dict || { echo "FAIL: imgdump generated engine"; exit 1; }
grep -q '^+ ' $T/hb-rt-dict || { echo "FAIL: imgdump missing seed dict"; exit 1; }
printf ': RBAD ( i64 -- i64 ) 0= ;\nEXPORT RBAD\n' > $T/hb-rt-bad.f
if ./tools/hb-build.sh --repl $T/hb-rt-bad.f -o $T/hb-rt-bad >/dev/null 2>$T/hb-rt-bad.err; then
  echo "FAIL: hb-build --repl accepted bool-as-i64 false cert"; exit 1
fi
grep -q "expected: i64" $T/hb-rt-bad.err || { echo "FAIL: hb-build --repl diagnostic lost expected type"; exit 1; }
grep -q "actual: bool" $T/hb-rt-bad.err || { echo "FAIL: hb-build --repl diagnostic lost actual type"; exit 1; }
echo "PASS: hb-build --repl verifies user defs ($(stat -f%z $T/hb-rt) B, engine + library)"
echo "PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)"
