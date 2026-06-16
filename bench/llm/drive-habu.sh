#!/bin/sh
# drive-habu.sh — Habu benchmark arms. The model writes a checked array word; we certify it
# (tools/check.sh), and on rejection feed the checker diagnostic back as the repair
# signal (up to N rounds). On certify, grade values via grade.sh (which builds the
# array in memory and runs the io-vectors). Emits one JSONL metrics row.
# Usage: drive-habu.sh <id> <name> <sig> <spec> <conv> <vectors> <a|lib> [maxr]
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
ID=$1 NAME=$2 SIG=$3 SPEC=$4 CONV=$5 VEC=$6 ARM=$7 MAXR=${8:-5}
CLAUDE=${CLAUDE:-claude}; MODEL=${MODEL:-claude}
case "$ARM" in
  a) PRE=$(cat bench/llm/habu-preamble.txt); LIB=0 ;;
  lib) PRE=$(cat bench/llm/habu-preamble-lib.txt); LIB=1 ;;
  *) echo "drive-habu: unknown arm $ARM" >&2; exit 64 ;;
esac
T=$(mktemp -d "${TMPDIR:-/tmp}/dh.XXXXXX"); trap 'rm -rf "$T"' EXIT
hb_test "$CONV" "$NAME" "$VEC" > "$T/vec.f"
cases=$(case_list "$VEC")
mode=$( [ "$CONV" = aa ] && echo "modify the array IN PLACE (write results back with !), returning nothing" || echo "return one integer" )
TASK="Define the word ${NAME} with signature:
  : ${NAME} ( ${SIG} ) ...
The input is an integer array passed as (pointer, length). ${SPEC}
For this task you must ${mode}."
extract() { sed 's/^```.*$//' "$1" | awk '/:/{if(!s)s=NR} {a[NR]=$0; if(/;/)e=NR} END{for(i=s;i<=e;i++)print a[i]}'; }
bundle() {
  if [ "$LIB" = 1 ]; then cat bench/llm/habu-array-lib.f "$T/cand.f" > "$T/bundle.f"
  else cp "$T/cand.f" "$T/bundle.f"; fi
}

round=0; feedback=""; outcome=reject; toks=0; t0=$(now_ms)
while [ "$round" -lt "$MAXR" ]; do
  round=$((round+1))
  prompt="${PRE}

${TASK}${feedback}"
  timeout 120 "$CLAUDE" -p "$prompt" --output-format json > "$T/resp.json" 2>/dev/null \
    || { outcome=error; break; }
  rt=$(node bench/llm/parse-resp.js "$T/resp.json" "$T/text.txt"); toks=$((toks+rt))
  extract "$T/text.txt" > "$T/cand.f"
  if ! grep -q ';' "$T/cand.f"; then
    feedback="

You produced no valid definition. Output ONLY the habu definition."
    outcome=reject; continue
  fi
  bundle
  if diag=$(tools/check.sh "$T/bundle.f" 2>&1); then
    outcome=$(sh bench/llm/grade.sh 5 "$T/bundle.f" "$T/vec.f")
    [ "$outcome" = pass ] && break
    feedback="

Your attempt:
$(cat "$T/cand.f")

It certified but FAILED the tests. It must satisfy (input -> expected):
${cases}
Fix the logic. Output ONLY the corrected definition."
  else
    feedback="

Your attempt:
$(cat "$T/cand.f")

The checker REJECTED it:
$(printf '%s' "$diag" | grep -v 'check.sh:')

Fix it so it certifies. Output ONLY the corrected definition."
    outcome=reject
  fi
done
wall=$(( $(now_ms) - t0 ))
emit_row "$ID" "$NAME" "$MODEL" "habu-$ARM" "$outcome" "$round" "$toks" "$wall"
