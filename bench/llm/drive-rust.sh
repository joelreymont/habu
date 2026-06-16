#!/bin/sh
# drive-rust.sh — Rust arm (array algorithms). f(a: &[i64]) returns i64 (conv as)
# or Vec<i64> (conv aa). Repair on rustc errors AND test failures.
# Usage: drive-rust.sh <id> <name> <sig> <spec> <conv> <vectors> [maxr]
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
ID=$1 NAME=$2 SIG=$3 SPEC=$4 CONV=$5 VEC=$6 MAXR=${7:-5}
CLAUDE=${CLAUDE:-claude}; MODEL=${MODEL:-claude}
asserts=$(rust_test "$CONV" "$VEC")
T=$(mktemp -d "${TMPDIR:-/tmp}/drs.XXXXXX"); trap 'rm -rf "$T"' EXIT
TASK="Write a Rust function with this exact signature:
  fn f(a: &[i64]) -> $(rust_ret "$CONV") { ... }
where a is a slice of integers. ${SPEC}
Output ONLY the function definition. No prose, no code fences."
extract() { sed 's/^```.*$//' "$1"; }

round=0; feedback=""; outcome=reject; toks=0; t0=$(now_ms)
while [ "$round" -lt "$MAXR" ]; do
  round=$((round+1))
  prompt="${TASK}${feedback}"
  timeout 120 "$CLAUDE" -p "$prompt" --output-format json > "$T/resp.json" 2>/dev/null \
    || { outcome=error; break; }
  rt=$(node bench/llm/parse-resp.js "$T/resp.json" "$T/text.txt"); toks=$((toks+rt))
  extract "$T/text.txt" > "$T/f.rs"
  {
    cat "$T/f.rs"; printf '\n'
    printf 'fn main() {\n%s\n    println!("ALL-OK");\n}\n' "$asserts"
  } > "$T/test.rs"
  if ! rustc "$T/test.rs" -o "$T/bin" 2>"$T/comp.err"; then
    outcome=reject
    feedback="

Your attempt:
$(cat "$T/f.rs")

rustc REJECTED it:
$(head -8 "$T/comp.err")

Fix it so it compiles. Output ONLY the corrected function."
    continue
  fi
  set +e; out=$(timeout 5 "$T/bin" 2>&1); rc=$?; set -e
  if [ "$rc" -eq 0 ] && printf '%s' "$out" | grep -q ALL-OK; then outcome=pass; break
  elif [ "$rc" -eq 124 ]; then outcome=timeout
  else outcome=fail; fi
  feedback="

Your attempt:
$(cat "$T/f.rs")

It compiled but FAILED a test:
$(printf '%s' "$out" | grep -i 'assert\|panic\|left\|right' | head -4)

Fix the logic. Output ONLY the corrected function."
done
wall=$(( $(now_ms) - t0 ))
emit_row "$ID" "$NAME" "$MODEL" "rust" "$outcome" "$round" "$toks" "$wall"
