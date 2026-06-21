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
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f test/gate-build-common.f test/gate-hb-build-repl.f || { echo "FAIL: native hb-build REPL gate phase"; exit 1; }
echo "PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)"
