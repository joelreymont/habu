#!/bin/sh
set -eu

base=${TMPDIR:-/tmp}
base=${base%/}
tmp=$(mktemp -d "$base/habu-public-signatures-test.XXXXXX")
trap 'rm -rf "$tmp"' EXIT INT TERM
HB=${HABU_HB:-bin/hb}

script="$tmp/public-signatures.f"
cat tools/lint/lib.f \
    tools/public-signatures.f > "$script"

fixture="$tmp/public-signatures-fixture.f"
cat > "$fixture" <<'EOF'
\ public signature fixture
EXPORT lower
EXPORT 1+
: lower (   x -- x   ) dup ;
: CAPS ( i64 [ i64 -- i64 ] -- i64 ) execute ;
: Mixed ( i64 -- i64 ) dup ;
: 1+ ( i64 -- i64 ) 1 + ;
: BAD ( i64 ) dup ;
s" : STRINGED ( i64 -- i64 ) dup ;"
( : COMMENTED ( i64 -- i64 ) dup ; )
EOF

habu_good="$tmp/good-habu.json"
"$HB" "$script" examples/llm/good.f > "$habu_good"

habu_fixture="$tmp/fixture-habu.json"
"$HB" "$script" "$fixture" > "$habu_fixture"

grep -Fq '"schema_version":1' "$habu_good"
grep -Fq '"word":"SQUARE"' "$habu_good"
grep -Fq '"signature":"(i64 -- i64)"' "$habu_good"
grep -Fq '"word":"APPLY"' "$habu_good"
grep -Fq '"signature":"(i64 [ i64 -- i64 ] -- i64)"' "$habu_good"

grep -Fq '"word":"LOWER"' "$habu_fixture"
grep -Fq '"signature":"(x -- x)"' "$habu_fixture"
grep -Fq '"exported":true' "$habu_fixture"
grep -Fq '"word":"CAPS"' "$habu_fixture"
grep -Fq '"exported":false' "$habu_fixture"
grep -Fq '"word":"1+"' "$habu_fixture"
! grep -Fq '"word":"MIXED"' "$habu_fixture"
! grep -Fq '"word":"BAD"' "$habu_fixture"

set +e
"$HB" "$script" < /dev/null > "$tmp/noargs.out" 2> "$tmp/noargs.err"
rc=$?
set -e
if [ "$rc" -eq 0 ]; then
  echo "public-signatures-test: expected no-arg failure" >&2
  exit 1
fi
if [ "$rc" -ne 64 ]; then
  echo "public-signatures-test: no-arg rc $rc, want 64" >&2
  exit 1
fi

echo "public-signatures-test: ok"
