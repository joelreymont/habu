#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

fail() {
  echo "FAIL: repair-schema-doc-test: $*" >&2
  exit 1
}

DOC=docs/repair-diagnostics.md
[ -f "$DOC" ] || fail "missing $DOC"

need_doc() {
  grep -Fq "$1" "$DOC" || fail "missing doc anchor: $1"
}

need_llm() {
  grep -Fq "$1" LLM.md || fail "missing LLM.md link/text: $1"
}

need_doc "# Repair Diagnostics Schema"
need_doc "## Checker Diagnostic JSON"
need_doc "## Repair Packet JSON"
need_doc "## Benchmark Result Fields"

for field in \
  schema_version code repair_class verdict word token token_index file line column \
  byte_start byte_end definition_source declared_effect inferred_effect \
  return_stack expected actual suggestion source_excerpt reason; do
  need_doc "| \`$field\` |"
done

for class in \
  remove_producer add_producer fix_type fix_return_stack \
  trusted_boundary_required fix_signature_syntax rewrite_uncheckable \
  unknown_rejection; do
  need_doc "\`$class\`"
done

need_llm "docs/repair-diagnostics.md"
need_llm "Repair diagnostic schema"

T=$(mktemp -d "${TMPDIR:-/tmp}/hb-repair-schema.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

ASSERT=$T/gate-json-assert.f
cat tools/json.f tools/gate-json-assert.f > "$ASSERT"

cat > "$T/bad.f" <<'EOF'
: JBAD ( i64 -- i64 ) dup ;
EOF

set +e
./tools/check.sh --json-errors --all-errors "$T/bad.f" >"$T/out" 2>"$T/err"
rc=$?
set -e
[ "$rc" -ne 0 ] || fail "checker accepted diagnostic fixture"

bin/hb "$ASSERT" json-lines-schema "$T/err"

for field in \
  schema_version code repair_class verdict word token token_index file line column \
  byte_start byte_end definition_source declared_effect inferred_effect \
  return_stack expected actual suggestion; do
  grep -Fq "\"$field\":" "$T/err" || fail "checker JSON missing $field"
done

grep -Fq '"repair_class":"remove_producer"' "$T/err" ||
  fail "checker JSON lost remove_producer class"

echo "PASS: repair-schema-doc-test"
