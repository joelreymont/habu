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
$G test/t-cg-effect.fs -e bye > /tmp/habu-cg-effect.log 2>&1 || { tail -5 /tmp/habu-cg-effect.log; echo "FAIL: t-cg-effect.fs"; exit 1; }
./bench/typed-codegen/run.sh > /tmp/habu-typed-codegen.json 2>/tmp/habu-typed-codegen.err || { tail -5 /tmp/habu-typed-codegen.err; echo "FAIL: typed-codegen-bench"; exit 1; }
python3 - /tmp/habu-typed-codegen.json <<'PY'
import json, pathlib, sys
doc = json.loads(pathlib.Path(sys.argv[1]).read_text())
assert doc["schema_version"] == 1, doc
fixtures = {item["category"]: item for item in doc["fixtures"]}
for category in ("bool_control", "arithmetic_loop", "quotation_call", "polymorphic_helper", "polymorphic_caller"):
    assert category in fixtures, doc
assert fixtures["polymorphic_helper"]["effect_flags"] == 0, fixtures["polymorphic_helper"]
assert fixtures["bool_control"]["text_bytes"] > 0, fixtures["bool_control"]
PY
$G test/selfhost-all.fs -e bye > /tmp/habu-shgate.log 2>&1 || { tail -5 /tmp/habu-shgate.log; echo "FAIL: selfhost-all.fs"; exit 1; }
( cd test && $G t-shake.fs -e bye ) > /tmp/habu-shake.log 2>&1 || { tail -5 /tmp/habu-shake.log; echo "FAIL: t-shake.fs"; exit 1; }
$G test/t-sh-jdiag.fs -e bye > /tmp/habu-jdiag.log 2>&1 || { tail -5 /tmp/habu-jdiag.log; echo "FAIL: t-sh-jdiag.fs"; exit 1; }
./bench/llm/run.sh > /tmp/habu-bench.log 2>&1 || { tail -5 /tmp/habu-bench.log; echo "FAIL: llm-bench"; exit 1; }
echo "PASS: oracle (all.fs + t-cg-effect + typed-codegen + selfhost-all.fs + t-shake + t-sh-jdiag + llm-bench)"
