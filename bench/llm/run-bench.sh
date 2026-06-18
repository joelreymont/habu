#!/bin/sh
# run-bench.sh [k_trials] [out.jsonl] — run every array task in the canonical
# manifest across all four arms
# {habu-a, habu-lib, js, rust} for k trials, appending JSONL metrics rows. Then
# report.js aggregates into RESULTS.md. Makes real `claude -p` calls.
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
K=${1:-2}
OUT=${2:-${BENCH_OUT:-bench/llm/results/run.jsonl}}
TASKS=${BENCH_TASKS:-bench/llm/tasks.tsv}
RESULTS=${BENCH_RESULTS:-bench/llm/RESULTS.md}
BENCH_SEED=${BENCH_SEED:-manifest}
mkdir -p "$(dirname "$OUT")"
: > "$OUT"
TAB=$(printf '\t')
bench_require_manifest_header "$TASKS"
task_order=0
tail -n +2 "$TASKS" | while IFS="$TAB" read -r id name signature category tests harness conv spec vectors tags js_signature rust_signature; do
  [ -n "$id" ] || continue
  [ "$harness" = array ] || continue
  task_order=$((task_order+1))
  sig=$(bench_sig "$signature")
  model_ids | while IFS= read -r model_id; do
    t=1
    while [ "$t" -le "$K" ]; do
      # Four arms — raw Habu, library-assisted Habu, JS, Rust.
      # </dev/null: model CLIs may otherwise read this loop's piped stdin and swallow
      # the remaining task lines. || true: a failing driver must not abort the sweep.
      MODEL_ID=$model_id BENCH_TRIAL=$t BENCH_TASK_ORDER=$task_order BENCH_K=$K BENCH_SEED=$BENCH_SEED sh bench/llm/drive-habu.sh "$id" "$name" "$sig" "$spec" "$conv" "$vectors" a </dev/null >> "$OUT" || true
      MODEL_ID=$model_id BENCH_TRIAL=$t BENCH_TASK_ORDER=$task_order BENCH_K=$K BENCH_SEED=$BENCH_SEED sh bench/llm/drive-habu.sh "$id" "$name" "$sig" "$spec" "$conv" "$vectors" lib </dev/null >> "$OUT" || true
      MODEL_ID=$model_id BENCH_TRIAL=$t BENCH_TASK_ORDER=$task_order BENCH_K=$K BENCH_SEED=$BENCH_SEED sh bench/llm/drive-js.sh   "$id" "$name" "$sig" "$spec" "$conv" "$vectors"   </dev/null >> "$OUT" || true
      MODEL_ID=$model_id BENCH_TRIAL=$t BENCH_TASK_ORDER=$task_order BENCH_K=$K BENCH_SEED=$BENCH_SEED sh bench/llm/drive-rust.sh "$id" "$name" "$sig" "$spec" "$conv" "$vectors"   </dev/null >> "$OUT" || true
      t=$((t+1))
    done
  done
  echo "[run-bench] task $id $name done (k=$K)" >&2
done
node bench/llm/report.js "$OUT" > "$RESULTS"
echo "[run-bench] wrote $RESULTS" >&2
