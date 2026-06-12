#!/bin/sh
# run.sh — the DEFAULT gate: habu-native, no gforth anywhere on the path.
#   lints -> self-rebuild fixpoint -> hb-suite -> AOT snapshot -> tty REPL ->
#   hb-build standalone.
# The gforth differential (boot-vs-port goldens + the gforth-hosted checker
# suite) lives in tools/oracle.sh — run it before pushing emitter changes.
# `test/run.sh full` runs both.
set -e
cd "$(dirname "$0")/.."
./tools/parity-lint.py || { echo "FAIL: parity-lint"; exit 1; }
./tools/shadow-lint.py || { echo "FAIL: shadow-lint"; exit 1; }
./tools/clobber-lint.py || { echo "FAIL: clobber-lint"; exit 1; }
[ -x bin/hb ] || { echo "no bin/hb — run tools/bootstrap.sh once"; exit 1; }
./tools/build.sh > /tmp/hb-build.log 2>&1 || { tail -5 /tmp/hb-build.log; echo "FAIL: build (fixpoint)"; exit 1; }
echo "PASS: self-rebuild fixpoint"
./tools/test-hb.sh || exit 1
./tools/snap-hb.sh >/dev/null || { echo "FAIL: snap-hb"; exit 1; }
out=$(echo 's" w" s" n -- n" trust 7 . : Q 5 dup * . ; Q' | /tmp/hb-warm)
[ "$out" = "7
25" ] || { echo "FAIL: warm snapshot (got: $out)"; exit 1; }
echo "PASS: AOT snapshot (warm toolchain boot)"
python3 test/repl-pty.py || { echo "FAIL: tty REPL"; exit 1; }
printf ': T 6 7 * . ;\nT\n' > /tmp/hb-bt.f
./tools/hb-build.sh /tmp/hb-bt.f -o /tmp/hb-bt >/dev/null || { echo "FAIL: hb-build"; exit 1; }
[ "$(/tmp/hb-bt)" = "42" ] || { echo "FAIL: hb-build output (got: $(/tmp/hb-bt))"; exit 1; }
echo "PASS: hb-build standalone"
if [ "$1" = "full" ]; then
  ./tools/oracle.sh || exit 1
fi
echo "PASS: native gate (fixpoint + hb-suite + snapshot + repl + hb-build)${1:+ + oracle}"
