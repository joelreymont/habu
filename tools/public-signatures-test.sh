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

python_good="$tmp/good-python.json"
habu_good="$tmp/good-habu.json"
tools/public-signatures.py examples/llm/good.f > "$python_good"
"$HB" "$script" examples/llm/good.f > "$habu_good"

python_fixture="$tmp/fixture-python.json"
habu_fixture="$tmp/fixture-habu.json"
tools/public-signatures.py "$fixture" > "$python_fixture"
"$HB" "$script" "$fixture" > "$habu_fixture"

python3 - "$python_good" "$habu_good" "$python_fixture" "$habu_fixture" <<'PY'
import json
import pathlib
import sys

paths = [pathlib.Path(p) for p in sys.argv[1:]]
good_py, good_habu, fixture_py, fixture_habu = [json.loads(p.read_text()) for p in paths]
assert good_habu == good_py, (good_habu, good_py)
assert fixture_habu == fixture_py, (fixture_habu, fixture_py)

items = {item["word"]: item for item in fixture_habu["definitions"]}
assert set(items) == {"LOWER", "CAPS", "1+"}, items
assert items["LOWER"]["signature"] == "(x -- x)", items["LOWER"]
assert items["LOWER"]["exported"] is True, items["LOWER"]
assert items["1+"]["exported"] is True, items["1+"]
assert items["CAPS"]["exported"] is False, items["CAPS"]
PY

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
