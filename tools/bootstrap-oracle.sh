#!/bin/sh
# bootstrap-oracle.sh — gforth bootstrap differential: the gforth-hosted checker suite
# (test/all.fs) and the boot-vs-port golden parity suite (test/selfhost-all.fs,
# the t-sh-* files that build engines with BOTH the gforth builder and the
# ported toolchain and compare them word for word).
#
# This is the only place outside tools/bootstrap.sh that needs gforth. It is
# bootstrap-only: use it when changing the bootstrap seed/reference mirror or
# validating recovery from no native binary. The default gate is Habu-native.
set -e
G=${GFORTH:-$HOME/.local/bin/gforth}
cd "$(dirname "$0")/.."
# hermetic gforth FFI cache: exec.fs's `system` goes through libcc; the per-user
# global cache (~/.cache/gforth) corrupts under concurrent gforths (hash
# mismatch / missing .so). Use a gate-owned cache and prime it serially.
export XDG_CACHE_HOME=/tmp/habu-gforth-cache
[ -d "$XDG_CACHE_HOME/gforth" ] || "$G" -e 's" true" system bye' >/dev/null 2>&1
$G test/all.fs -e bye > /tmp/habu-gate.log 2>&1 || { tail -5 /tmp/habu-gate.log; echo "FAIL: all.fs"; exit 1; }
$G test/t-cg-effect.fs -e bye > /tmp/habu-cg-effect.log 2>&1 || { tail -5 /tmp/habu-cg-effect.log; echo "FAIL: t-cg-effect.fs"; exit 1; }
$G test/selfhost-all.fs -e bye > /tmp/habu-shgate.log 2>&1 || { tail -5 /tmp/habu-shgate.log; echo "FAIL: selfhost-all.fs"; exit 1; }
( cd test && $G t-shake.fs -e bye ) > /tmp/habu-shake.log 2>&1 || { tail -5 /tmp/habu-shake.log; echo "FAIL: t-shake.fs"; exit 1; }
$G test/t-sh-jdiag.fs -e bye > /tmp/habu-jdiag.log 2>&1 || { tail -5 /tmp/habu-jdiag.log; echo "FAIL: t-sh-jdiag.fs"; exit 1; }
./bench/llm/run.sh > /tmp/habu-bench.log 2>&1 || { tail -5 /tmp/habu-bench.log; echo "FAIL: llm-bench"; exit 1; }
echo "PASS: bootstrap oracle (all.fs + t-cg-effect + selfhost-all.fs + t-shake + t-sh-jdiag + llm-bench)"
