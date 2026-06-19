#!/bin/sh
# Focused tests for tools/checked-boundary-lint.f.
set -eu

cd "$(dirname "$0")/.."
ROOT=$(pwd)
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-checked-boundary-lint.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

BUNDLE=$T/checked-boundary-lint.bundle.f
cat "$ROOT/lib/errors.f" \
    "$ROOT/lib/string.f" \
    "$ROOT/lib/fs.f" \
    "$ROOT/tools/argv.f" \
    "$ROOT/tools/checked-boundary-lint.f" > "$BUNDLE"

run_lint() {
  "$ROOT/bin/hb" "$BUNDLE" "$@"
}

out=$(run_lint \
  "$ROOT/tools/checked-boundary-lint.f" \
  "$ROOT/bench/llm/report.f" \
  "$ROOT/bench/llm/parse-resp.f" \
  "$ROOT/bench/llm/validate-results.f" \
  "$ROOT/tools/host-lint.f" \
  "$ROOT/tools/filemap-lint.f" \
  "$ROOT/tools/parallel-agent-lint.f" \
  "$ROOT/tools/signature-lint.f" \
  "$ROOT/tools/stale-status-lint.f" \
  "$ROOT/tools/trust-lint.f")
[ -z "$out" ] || {
  echo "FAIL: checked-boundary-lint protected files emitted output: $out"
  exit 1
}

cat > "$T/good.f" <<'EOF'
0 set-check
variable RAW-CELL
: GOOD-CHECK-HOOK ( -- ) CHECK! ;
' GOOD-CHECK-HOOK set-check
: GOOD ( n -- n ) dup ;
EOF
out=$(run_lint "$T/good.f")
[ -z "$out" ] || {
  echo "FAIL: checked-boundary-lint good fixture emitted output: $out"
  exit 1
}

cat > "$T/bad.f" <<'EOF'
0 set-check
: BAD ( n -- n ) dup ;
EOF
set +e
out=$(run_lint "$T/bad.f" 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || {
  echo "FAIL: checked-boundary-lint accepted broad unchecked definition"
  exit 1
}
printf '%s\n' "$out" | grep -q 'UNCHECKED-DEFINITION' || {
  echo "FAIL: checked-boundary-lint missing diagnostic"
  printf '%s\n' "$out"
  exit 1
}

echo "PASS: checked-boundary-lint fixtures"
