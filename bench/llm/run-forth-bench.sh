#!/bin/sh
# run-forth-bench.sh [k_trials] [out.jsonl] — run every harness=forth task in
# the canonical manifest through the live Habu Forth driver. By default it runs
# the diagnostic ablation arms: structured repair packets, raw diagnostics, and
# blind failure feedback.
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
if [ -n "${BENCH_FORTH_MODES:-}" ]; then
  FEEDBACK_MODES=$BENCH_FORTH_MODES
elif [ -n "${BENCH_FORTH_ARM:-}" ]; then
  FEEDBACK_MODES=repair
else
  FEEDBACK_MODES="repair raw blind"
fi
T=$(mktemp -d "${TMPDIR:-/tmp}/run-forth-bench.XXXXXX")
trap 'rm -rf "$T"' EXIT HUP INT TERM
EXPECTED=$T/expected.tsv
: > "$EXPECTED"

mkdir -p "$(dirname "$OUT")"
if [ "${BENCH_RESUME:-0}" = 1 ] && [ -f "$OUT" ]; then
  :
else
  : > "$OUT"
fi
bench_require_manifest_header "$TASKS"

mode_count=0
for mode in $FEEDBACK_MODES; do
  mode_count=$((mode_count + 1))
done
[ "$mode_count" -gt 0 ] || { echo "run-forth-bench: no BENCH_FORTH_MODES selected" >&2; exit 64; }
if [ -n "${BENCH_FORTH_ARM:-}" ] && [ "$mode_count" -ne 1 ]; then
  echo "run-forth-bench: BENCH_FORTH_ARM requires exactly one BENCH_FORTH_MODES entry" >&2
  exit 64
fi

arm_for_mode() {
  mode=$1
  if [ -n "${BENCH_FORTH_ARM:-}" ]; then
    printf '%s\n' "$BENCH_FORTH_ARM"
    return 0
  fi
  case "$mode" in
    repair) printf 'habu-forth\n' ;;
    raw) printf 'habu-forth-raw\n' ;;
    blind) printf 'habu-forth-blind\n' ;;
    *) echo "run-forth-bench: unknown feedback mode $mode" >&2; return 64 ;;
  esac
}

task_selected() {
  id=$1
  [ -n "${BENCH_TASK_IDS:-}" ] || return 0
  case ",$BENCH_TASK_IDS," in
    *",$id,"*) return 0 ;;
    *) return 1 ;;
  esac
}

row_done() {
  rid=$1
  rmodel=$2
  rarm=$3
  rtrial=$4
  [ -f "$OUT" ] || return 1
  grep -F "\"task_id\":$rid," "$OUT" |
    grep -F "\"model_id\":\"$rmodel\"" |
    grep -F "\"arm\":\"$rarm\"" |
    grep -F "\"trial\":$rtrial," >/dev/null
}

expect_row() {
  printf '%s\t%s\t%s\t%s\n' "$1" "$2" "$3" "$4" >> "$EXPECTED"
}

check_expected_rows() {
  missing=0
  while IFS="$TAB" read -r eid emodel earm etrial; do
    [ -n "$eid" ] || continue
    if ! row_done "$eid" "$emodel" "$earm" "$etrial"; then
      echo "run-forth-bench: missing row task=$eid model=$emodel arm=$earm trial=$etrial" >&2
      missing=$((missing + 1))
    fi
  done < "$EXPECTED"
  [ "$missing" -eq 0 ] || {
    echo "run-forth-bench: $missing missing row(s); rerun with BENCH_RESUME=1 after fixing the driver" >&2
    return 1
  }
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
    for mode in $FEEDBACK_MODES; do
      arm=$(arm_for_mode "$mode") || exit $?
      t=1
      while [ "$t" -le "$K" ]; do
        expect_row "$id" "$model_id" "$arm" "$t"
        if [ "${BENCH_RESUME:-0}" = 1 ] && row_done "$id" "$model_id" "$arm" "$t"; then
          t=$((t + 1))
          continue
        fi
        MODEL_ID=$model_id \
        BENCH_TRIAL=$t \
        BENCH_TASK_ORDER=$task_order \
        BENCH_K=$K \
        BENCH_SEED=$BENCH_SEED \
        BENCH_TASK_FAMILY=$category \
        BENCH_FORTH_FEEDBACK=$mode \
        BENCH_FORTH_ARM=$arm \
        sh bench/llm/drive-forth.sh "$id" "$name" "$sig" "$category" "$tests" "$spec" "$MAXR" </dev/null >> "$OUT" || true
        t=$((t + 1))
      done
    done
  done
  echo "[run-forth-bench] task $id $name done (k=$K modes=$FEEDBACK_MODES)" >&2
done

check_expected_rows
sh bench/llm/expanded-report.sh "$OUT" > "$RESULTS"
echo "[run-forth-bench] wrote $RESULTS" >&2
