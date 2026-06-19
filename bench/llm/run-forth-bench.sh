#!/bin/sh
# run-forth-bench.sh [k_trials] [out.jsonl] — run every harness=forth task in
# the canonical manifest through the live Habu Forth driver.
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh

K=${1:-5}
OUT=${2:-${BENCH_OUT:-bench/llm/results/run-expanded.jsonl}}
TASKS=${BENCH_TASKS:-bench/llm/tasks.tsv}
RESULTS=${BENCH_RESULTS:-bench/llm/RESULTS-expanded.md}
BENCH_SEED=${BENCH_SEED:-manifest}
MAXR=${BENCH_MAX_REPAIRS:-5}
TAB=$(printf '\t')

mkdir -p "$(dirname "$OUT")"
: > "$OUT"
bench_require_manifest_header "$TASKS"

task_selected() {
  id=$1
  [ -n "${BENCH_TASK_IDS:-}" ] || return 0
  case ",$BENCH_TASK_IDS," in
    *",$id,"*) return 0 ;;
    *) return 1 ;;
  esac
}

task_order=0
selected=0
tail -n +2 "$TASKS" | while IFS="$TAB" read -r id name signature category tests harness conv spec vectors tags js_signature rust_signature; do
  [ -n "$id" ] || continue
  [ "$harness" = forth ] || continue
  task_order=$((task_order + 1))
  task_selected "$id" || continue
  if [ -n "${BENCH_TASK_LIMIT:-}" ] && [ "$selected" -ge "$BENCH_TASK_LIMIT" ]; then
    break
  fi
  selected=$((selected + 1))
  sig=$(bench_sig "$signature")
  model_ids | while IFS= read -r model_id; do
    [ -n "$model_id" ] || continue
    t=1
    while [ "$t" -le "$K" ]; do
      MODEL_ID=$model_id \
      BENCH_TRIAL=$t \
      BENCH_TASK_ORDER=$task_order \
      BENCH_K=$K \
      BENCH_SEED=$BENCH_SEED \
      BENCH_TASK_FAMILY=$category \
      sh bench/llm/drive-forth.sh "$id" "$name" "$sig" "$category" "$tests" "$spec" "$MAXR" </dev/null >> "$OUT" || true
      t=$((t + 1))
    done
  done
  echo "[run-forth-bench] task $id $name done (k=$K)" >&2
done

sh bench/llm/expanded-report.sh "$OUT" > "$RESULTS"
echo "[run-forth-bench] wrote $RESULTS" >&2
