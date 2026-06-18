#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

fail() {
  echo "FAIL: $1"
  exit 1
}

[ -x bin/hb ] || fail "bin/hb missing or not executable"
[ ! -e bin/hbi ] || fail "legacy public binary bin/hbi exists"
[ ! -e bin/habu ] || fail "legacy public binary bin/habu exists"

public_count=0
public_extra=""
for f in bin/*; do
  [ -e "$f" ] || continue
  [ -f "$f" ] || continue
  [ -x "$f" ] || continue
  public_count=$((public_count + 1))
  [ "$f" = "bin/hb" ] || public_extra="${public_extra} $f"
done
[ "$public_count" -eq 1 ] || fail "expected one public executable in bin, found $public_count"
[ -z "$public_extra" ] || fail "unexpected public executable(s):$public_extra"

T=$(mktemp -d "${TMPDIR:-/tmp}/hb-baseline-contracts.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

out=$(printf '41 1 + . cr\n' | bin/hb 2>/dev/null)
[ "$out" = "42" ] || fail "bin/hb invocation output (got: $out)"

cat > "$T/script-mode.f" <<'EOF'
." SCRIPT" cr
SCRIPT-ARGC .
0 SCRIPT-ARGV$ type cr
EOF

out=$(printf '." PIPE" cr\n' | bin/hb "$T/script-mode.f" 2>/dev/null)
[ "$out" = "PIPE" ] || fail "pipeline stdin did not win over argv script (got: $out)"

out=$(bin/hb "$T/script-mode.f" omega < /dev/null 2>/dev/null)
[ "$out" = "SCRIPT
1
omega" ] || fail "empty non-tty stdin did not run argv script (got: $out)"

bin/hb 123 4 < test/prop-test.f > "$T/prop-argv.out" 2> "$T/prop-argv.err" ||
  fail "prop-test rejected seed/count argv"
prop_norm=$(tr '\n' ' ' < "$T/prop-argv.out")
case "$prop_norm" in
  *"prop-test: self-test OK"*) ;;
  *) fail "prop-test seed/count argv self-test missing" ;;
esac
case "$prop_norm" in
  *"prop-test: 4 programs,"*) ;;
  *) fail "prop-test seed/count argv count missing" ;;
esac

echo "PASS: hb baseline contracts (bin/hb only, stdin/script modes, prop argv)"
