#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
ROOT=$(pwd)
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-check-all.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

cat > "$T/input.f" <<'EOF'
: OK ( i64 -- i64 ) dup * ;
: SEMI ( -- i64 ) [char] ; ;
: BAD1 ( i64 -- i64 ) dup ;
: BAD2 ( i64 -- ) >r ;
EOF

set +e
"$ROOT/bin/hb" --load "$ROOT/tools/lint/lib.f" "$ROOT/tools/lint/json-writer.f" \
  "$ROOT/tools/lint/source-lex.f" "$ROOT/tools/argv.f" "$ROOT/tools/check-all-errors.f" \
  -- --json-errors --label "$T/input.f" "$T/input.f" >"$T/out" 2>"$T/err"
rc=$?
set -e
[ "$rc" -eq 70 ] || {
  echo "FAIL: check-all-errors rc $rc"
  cat "$T/err"
  exit 1
}
grep -q '"word":"bad1"' "$T/err" || { echo "FAIL: missing BAD1 JSON"; cat "$T/err"; exit 1; }
grep -q '"word":"bad2"' "$T/err" || { echo "FAIL: missing BAD2 JSON"; cat "$T/err"; exit 1; }
lines=$(wc -l < "$T/err" | tr -d ' ')
[ "$lines" = 2 ] || { echo "FAIL: expected 2 JSON lines, got $lines"; cat "$T/err"; exit 1; }

echo "PASS: check-all-errors fixtures"
