#!/bin/sh
# run-bench.sh [k_trials] — run every task in bench-tasks.tsv across all four arms
# {habu-a, habu-lib, js, rust} for k trials, appending JSONL metrics rows. Then
# report.js aggregates into RESULTS.md. Makes real `claude -p` calls.
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
K=${1:-2}
OUT=bench/llm/results/run.jsonl
mkdir -p bench/llm/results
: > "$OUT"
TAB=$(printf '\t')
tail -n +2 bench/llm/bench-tasks.tsv | while IFS="$TAB" read -r id name sig conv spec vectors; do
  [ -n "$id" ] || continue
  model_ids | while IFS= read -r model_id; do
    t=1
    while [ "$t" -le "$K" ]; do
      # Four arms — raw Habu, library-assisted Habu, JS, Rust.
      # </dev/null: model CLIs may otherwise read this loop's piped stdin and swallow
      # the remaining task lines. || true: a failing driver must not abort the sweep.
      MODEL_ID=$model_id sh bench/llm/drive-habu.sh "$id" "$name" "$sig" "$spec" "$conv" "$vectors" a </dev/null >> "$OUT" || true
      MODEL_ID=$model_id sh bench/llm/drive-habu.sh "$id" "$name" "$sig" "$spec" "$conv" "$vectors" lib </dev/null >> "$OUT" || true
      MODEL_ID=$model_id sh bench/llm/drive-js.sh   "$id" "$name" "$sig" "$spec" "$conv" "$vectors"   </dev/null >> "$OUT" || true
      MODEL_ID=$model_id sh bench/llm/drive-rust.sh "$id" "$name" "$sig" "$spec" "$conv" "$vectors"   </dev/null >> "$OUT" || true
      t=$((t+1))
    done
  done
  echo "[run-bench] task $id $name done (k=$K)" >&2
done
node bench/llm/report.js "$OUT" > bench/llm/RESULTS.md
echo "[run-bench] wrote bench/llm/RESULTS.md" >&2
