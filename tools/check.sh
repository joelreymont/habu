#!/bin/sh
# check.sh — feed a program to the checked native engine. `--json-errors`
# switches reject diagnostics from prose to structured JSON before user code runs.
set -e
cd "$(dirname "$0")/.."
JSON=0
STRICT=0
ALL=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    --json-errors) JSON=1; shift ;;
    --strict-signatures) STRICT=1; shift ;;
    --all-errors) ALL=1; shift ;;
    --) shift; break ;;
    -*) echo "usage: tools/check.sh [--json-errors] [--strict-signatures] [--all-errors] [prog.f]"; exit 64 ;;
    *) break ;;
  esac
done
[ "$#" -le 1 ] || { echo "usage: tools/check.sh [--json-errors] [--strict-signatures] [--all-errors] [prog.f]"; exit 64; }
[ -x bin/hb ] || { echo "check.sh: bin/hb missing (run tools/build.sh first)"; exit 69; }
T=$(mktemp -d "${TMPDIR:-/tmp}/habu-check.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM
SRC=$T/source.f
RUN=$T/run.f
if [ "$JSON" = 1 ]; then
  LINT_JSON=--json
else
  LINT_JSON=
fi
if [ "$#" = 1 ]; then
  [ -f "$1" ] || { echo "check.sh: no such source: $1"; exit 66; }
  LABEL=$1
  cp "$1" "$SRC"
else
  LABEL="<stdin>"
  cat > "$SRC"
fi
SIGNATURE_LINT_TOOL=$T/signature-lint.f
signature_lint() {
  [ -f "$SIGNATURE_LINT_TOOL" ] || cat tools/lint/lib.f tools/lint/source-lex.f tools/argv.f tools/signature-lint.f > "$SIGNATURE_LINT_TOOL"
  bin/hb "$SIGNATURE_LINT_TOOL" "$@"
}
DIAG_ORIGIN_TOOL=$T/diag-origin.f
diag_origin() {
  [ -f "$DIAG_ORIGIN_TOOL" ] || cat tools/lint/lib.f tools/diag-origin.f > "$DIAG_ORIGIN_TOOL"
  bin/hb "$DIAG_ORIGIN_TOOL" "$1"
}
if [ "$STRICT" = 1 ]; then
  signature_lint $LINT_JSON --label "$LABEL" "$SRC" >&2
fi
JSON_ONLY_TOOL=$T/json-only.f
json_only() {
  [ -f "$JSON_ONLY_TOOL" ] || cat tools/argv.f tools/json.f tools/json-only.f > "$JSON_ONLY_TOOL"
  bin/hb "$JSON_ONLY_TOOL" "$1"
}
if [ "$ALL" = 1 ]; then
  if [ "$JSON" = 1 ]; then JSON_ARG=--json-errors; else JSON_ARG=; fi
  ./tools/check-all-errors.py $JSON_ARG --label "$LABEL" "$SRC"
  exit $?
fi
case "$LABEL" in
  *\"*) echo "check.sh: source path contains a double quote, cannot set DIAG-FILE"; exit 64 ;;
esac
printf '%s\n' '0 set-check' > "$RUN"
printf 's" %s" DIAG-FILE!\n' "$LABEL" >> "$RUN"
if [ "$JSON" = 1 ]; then
  printf '%s\n' '-1 JSON-DIAGS !' >> "$RUN"
fi
cat >> "$RUN" <<'EOF'
: CHECK-SH-HOOK ( n n -- n )
   CHECK!  dup -1 <> IF s" check.sh: check did not certify" 70 die THEN ;
' CHECK-SH-HOOK set-check
EOF
diag_origin "$SRC" >> "$RUN"
if [ "$JSON" = 1 ]; then
  ERR=$T/stderr
  if bin/hb < "$RUN" 2>"$ERR"; then
    cat "$ERR" >&2
  else
    rc=$?
    json_only "$ERR" >&2
    exit "$rc"
  fi
else
  bin/hb < "$RUN"
fi
