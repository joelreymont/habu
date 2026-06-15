#!/bin/sh
# hb-build.sh — compile a Forth program into a standalone signed macOS binary.
#   tools/hb-build.sh prog.f -o out          AOT (default): compile MAIN to native,
#                                            strip the engine. Program must define `: MAIN ;`.
#   tools/hb-build.sh --repl prog.f -o out   verify, then bundle the full engine +
#                                            the program's definitions and REPL.
# In --repl mode the textual tree-shaker keeps every word NAMED in the source;
# add `EXPORT word1 word2 …` lines to keep extra words callable at the REPL.
# The output needs neither hb nor gforth to run.
set -e
cd "$(dirname "$0")/.."
REPL=0
JSON=0
STRICT=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    --repl) REPL=1; shift ;;
    --json-errors) JSON=1; shift ;;
    --strict-signatures) STRICT=1; shift ;;
    --) shift; break ;;
    -*) echo "usage: hb-build.sh [--repl] [--json-errors] [--strict-signatures] prog.f -o out"; exit 64 ;;
    *) break ;;
  esac
done
CLEAN_T=0
if [ -n "${HB_TMP:-}" ]; then
  T=$HB_TMP
else
  T=$(mktemp -d "${TMPDIR:-/tmp}/hb-build.XXXXXX")
  CLEAN_T=1
fi
mkdir -p "$T"
export HB_TMP=$T
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM
SRC=$1
[ "$2" = "-o" ] && [ -n "$3" ] || { echo "usage: hb-build.sh [--repl] [--json-errors] [--strict-signatures] prog.f -o out"; exit 64; }
OUT=$3
[ -f "$SRC" ] || { echo "hb-build: no such source: $SRC"; exit 66; }
[ -x bin/hb ] || { echo "hb-build: bin/hb missing (run tools/build.sh first)"; exit 69; }
case "$SRC" in
  *\"*) echo "hb-build: source path contains a double quote, cannot set DIAG-FILE"; exit 64 ;;
esac
if [ "$JSON" = 1 ]; then LINT_JSON=--json; else LINT_JSON=; fi
if [ "$STRICT" = 1 ]; then
  ./tools/signature-lint.py $LINT_JSON "$SRC" >&2
fi
if [ "$REPL" = 0 ]; then
  ./tools/aot-lint.py $LINT_JSON "$SRC" >&2
fi

if [ "$REPL" = 1 ]; then DRIVER=build; ISRC=$T/hb-build-src; GOT=hb-build-got; MK=hb-build-mk
else                     DRIVER=aot;   ISRC=$T/hb-aot-src;   GOT=hb-aot-got;   MK=hb-aot-mk; fi
STAGE2_SRC=$T/stage2-src
STAGE2_GOT=$T/stage2-got
USRC=$T/hb-user-src
MKPATH=$T/$MK
GOTPATH=$T/$GOT
JSON_ONLY_TOOL=$T/json-only.f
json_only() {
  [ -f "$JSON_ONLY_TOOL" ] || cat tools/argv.f tools/json.f tools/json-only.f > "$JSON_ONLY_TOOL"
  bin/hb "$JSON_ONLY_TOOL" "$1"
}

# maker = checker-hooked toolchain + the chosen driver, compiled by bin/hb.
# In default AOT mode the driver compiles the program in-process under CHECK!.
# In --repl mode the driver pre-verifies the user source with CHECK!, then
# bundles that source plus trusted REPL support for startup/runtime execution.
{
  printf '0 set-check\n'
  for f in $(./tools/srclist.sh $DRIVER); do
    [ "$f" = "src/core/sha256.f" ] && printf ': HOOK CHECK ; '"'"' HOOK set-check\n'
    if [ "$f" = "src/habu/$DRIVER.f" ]; then
      sed '$d' "$f"
      printf 's" %s" DIAG-FILE!\n' "$SRC"
      [ "$JSON" = 1 ] && printf '%s\n' '-1 JSON-DIAGS !'
      tail -n 1 "$f"
    else
      cat "$f"
    fi
    printf '\n'
  done
} > "$STAGE2_SRC"
rm -f "$STAGE2_GOT"
bin/hb < src/habu/stage2.f
[ -f "$STAGE2_GOT" ] || { echo "hb-build: bootstrap maker did not produce stage2-got"; exit 74; }
mv "$STAGE2_GOT" "$MKPATH"
chmod +x "$MKPATH"

# the program. EXPORT lines are commented out (the names stay in the source text
# so the tree-shaker keeps them, but they don't execute). --repl keeps a
# user-only copy for build-time verification, then appends repl.f so the bundle
# installs the interactive REPL on a tty.
if [ "$REPL" = 1 ]; then
  ./tools/diag-origin.py "$SRC" > "$USRC"
  sed 's/^[[:space:]]*EXPORT /\\ EXPORT /' "$USRC" > "$T/hb-build-check-src"
  sed 's/^[[:space:]]*EXPORT /\\ EXPORT /' "$SRC" > "$ISRC"
  printf '\n' >> "$ISRC"
  cat src/habu/repl.f >> "$ISRC"
else
  ./tools/diag-origin.py "$SRC" > "$USRC"
  sed 's/^[[:space:]]*EXPORT /\\ EXPORT /' "$USRC" > "$ISRC"
fi
rm -f "$GOTPATH"
if [ "$JSON" = 1 ]; then
  ERR=$T/maker-stderr
  if "$MKPATH" 2>"$ERR"; then
    cat "$ERR" >&2
  else
    rc=$?
    json_only "$ERR" >&2
    exit "$rc"
  fi
else
  "$MKPATH"
fi
[ -f "$GOTPATH" ] || { echo "hb-build: maker did not produce $GOT"; exit 74; }
mv "$GOTPATH" "$OUT"
chmod +x "$OUT"
echo "hb-build OK: $OUT ($(stat -f%z "$OUT") B, $([ "$REPL" = 1 ] && echo 'engine+REPL bundle' || echo 'AOT — engine stripped'))"
