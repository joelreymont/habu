#!/bin/sh
# oracle.sh — the gforth DIFFERENTIAL gate: the gforth-hosted checker suite
# (test/all.fs) and the boot-vs-port golden parity suite (test/selfhost-all.fs,
# the t-sh-* files that build engines with BOTH the gforth builder and the
# ported toolchain and compare them word for word).
#
# This is the only place outside tools/bootstrap.sh that needs gforth. The
# default gate (test/run.sh) is habu-native; run THIS before pushing changes
# to the emitters (bootstrap/cg/* or src/**), or any time you want the
# cross-check. CI-equivalent: run.sh && oracle.sh.
set -e
G=${GFORTH:-$HOME/.local/bin/gforth}
cd "$(dirname "$0")/.."
# hermetic gforth FFI cache: exec.fs's `system` goes through libcc; the per-user
# global cache (~/.cache/gforth) corrupts under concurrent gforths (hash
# mismatch / missing .so). Use a gate-owned cache and prime it serially.
export XDG_CACHE_HOME=/tmp/habu-gforth-cache
[ -d "$XDG_CACHE_HOME/gforth" ] || "$G" -e 's" true" system bye' >/dev/null 2>&1
$G test/all.fs -e bye > /tmp/habu-gate.log 2>&1 || { tail -5 /tmp/habu-gate.log; echo "FAIL: all.fs"; exit 1; }
$G test/selfhost-all.fs -e bye > /tmp/habu-shgate.log 2>&1 || { tail -5 /tmp/habu-shgate.log; echo "FAIL: selfhost-all.fs"; exit 1; }
( cd test && $G t-shake.fs -e bye ) > /tmp/habu-shake.log 2>&1 || { tail -5 /tmp/habu-shake.log; echo "FAIL: t-shake.fs"; exit 1; }
$G test/t-sh-jdiag.fs -e bye > /tmp/habu-jdiag.log 2>&1 || { tail -5 /tmp/habu-jdiag.log; echo "FAIL: t-sh-jdiag.fs"; exit 1; }
echo "PASS: oracle (all.fs + selfhost-all.fs + t-shake + t-sh-jdiag)"
