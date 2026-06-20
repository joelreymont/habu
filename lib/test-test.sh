#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}

fail() {
  echo "test-test: $*" >&2
  exit 1
}

[ -x "$HB" ] || fail "missing executable $HB"
[ -f lib/test.f ] || fail "missing lib/test.f"

out=$("$HB" --load lib/test.f lib/test-test.f)
[ "$out" = "test: ok" ] || fail "unexpected success output: $out"

tmp=$(mktemp -d "${TMPDIR:-/tmp}/hb-test-lib.XXXXXX")
cleanup() {
  rm -rf "$tmp"
}
trap cleanup EXIT HUP INT TERM

cat lib/test.f > "$tmp/fail.f"
cat >> "$tmp/fail.f" <<'EOF'
T-RESET
1 2 T=
T-REPORT
EOF

if "$HB" "$tmp/fail.f" >"$tmp/fail.out" 2>"$tmp/fail.err"; then
  fail "failing assertion returned success"
fi

grep -Fq 'test: failures' "$tmp/fail.out" || fail "missing failure summary"

cat lib/test.f > "$tmp/throw.f"
cat >> "$tmp/throw.f" <<'EOF'
: THROW-5 ( -- ) 5 throw ;
T-RESET
' THROW-5 4 TTHROWS
T-REPORT
EOF

if "$HB" "$tmp/throw.f" >"$tmp/throw.out" 2>"$tmp/throw.err"; then
  fail "wrong throw assertion returned success"
fi

grep -Fq 'test: failures' "$tmp/throw.out" || fail "missing throw failure summary"

echo "test-test: ok"
