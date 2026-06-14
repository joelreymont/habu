#!/bin/sh
# check.sh — feed a program to the checked native engine. `--json-errors`
# switches reject diagnostics from prose to structured JSON before user code runs.
set -e
cd "$(dirname "$0")/.."
JSON=0
STRICT=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    --json-errors) JSON=1; shift ;;
    --strict-signatures) STRICT=1; shift ;;
    --) shift; break ;;
    -*) echo "usage: tools/check.sh [--json-errors] [--strict-signatures] [prog.f]"; exit 64 ;;
    *) break ;;
  esac
done
[ "$#" -le 1 ] || { echo "usage: tools/check.sh [--json-errors] [--strict-signatures] [prog.f]"; exit 64; }
[ -x bin/habu ] || { echo "check.sh: bin/habu missing (run tools/snap-hb.sh first)"; exit 69; }
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
if [ "$STRICT" = 1 ]; then
  ./tools/signature-lint.py $LINT_JSON --label "$LABEL" "$SRC" >&2
fi
case "$LABEL" in
  *\"*) echo "check.sh: source path contains a double quote, cannot set DIAG-FILE"; exit 64 ;;
esac
printf 's" %s" DIAG-FILE!\n' "$LABEL" > "$RUN"
if [ "$JSON" = 1 ]; then
  printf '%s\n' '-1 JSON-DIAGS !' >> "$RUN"
fi
cat "$SRC" >> "$RUN"
bin/habu < "$RUN"
