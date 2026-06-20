#!/bin/sh
# run-attempts.sh CANDIDATE_DIR [out.jsonl] [run_id] [model]
#
# Candidate layout:
#   CANDIDATE_DIR/1.f          one attempt for task 1
#   CANDIDATE_DIR/1/1.f        repair round 1 for task 1
#   CANDIDATE_DIR/1/2.f        repair round 2 for task 1
#
# Emits one validate-results.f-compatible JSONL row per harness=forth task.
set -eu
cd "$(dirname "$0")/../.."
CHECK="bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f tools/argv.f tools/check.f --"

[ "$#" -ge 1 ] || { echo "usage: bench/llm/run-attempts.sh CANDIDATE_DIR [out.jsonl] [run_id] [model]" >&2; exit 64; }
CAND_ROOT=$1
OUT=${2:-bench/llm/results/attempt.jsonl}
RUN_ID=${3:-attempt-$(date +%F)}
MODEL=${4:-candidate-dir}

[ -d "$CAND_ROOT" ] || { echo "run-attempts: no such candidate dir: $CAND_ROOT" >&2; exit 66; }
[ -x bin/hb ] || { echo "run-attempts: bin/hb missing (install a trusted seed with tools/seed.sh /path/to/hb)" >&2; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-attempts.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

REF=$T/ref
TASK_LINES=$T/tasks.body
mkdir -p "$REF" "$(dirname "$OUT")"
awk -F '\t' 'NR > 1 && $6 == "forth"' bench/llm/tasks.tsv > "$TASK_LINES"
awk -v dir="$REF" '
  BEGIN { FS = "\t" }
  FNR == NR {
    if (FNR > 1 && $6 == "forth") {
      task_id[$2] = $1
    }
    next
  }
  /^: / {
    split($0, parts, /[ \t]+/)
    name = parts[2]
    if (!(name in task_id)) {
      print "run-attempts: solution without harness=forth task: " name > "/dev/stderr"
      exit 1
    }
    print > (dir "/" task_id[name] ".f")
  }
' bench/llm/tasks.tsv bench/llm/solutions.f
while IFS="$(printf '\t')" read -r id _rest; do
  [ -n "$id" ] || continue
  [ -f "$REF/$id.f" ] || { echo "run-attempts: missing reference solution for task $id" >&2; exit 66; }
done < "$TASK_LINES"
: > "$OUT"

json_escape() {
  printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g'
}

norm_sig() {
  printf '%s' "$1" | sed 's/^[(]//; s/[)]$//' | xargs
}

file_sig() {
  awk '/^:/ {
    s = index($0, "("); e = index($0, ")");
    if (s > 0 && e > s) { print substr($0, s + 1, e - s - 1); exit }
  }' "$1" | xargs
}

bool() {
  [ "$1" = 1 ] && printf true || printf false
}

candidates_for() {
  id=$1
  if [ -d "$CAND_ROOT/$id" ]; then
    set -- "$CAND_ROOT/$id"/*.f
    [ -f "$1" ] || { echo "run-attempts: no candidate rounds for task $id" >&2; exit 66; }
    printf '%s\n' "$@"
  elif [ -f "$CAND_ROOT/$id.f" ]; then
    printf '%s\n' "$CAND_ROOT/$id.f"
  else
    echo "run-attempts: missing candidate for task $id" >&2
    exit 66
  fi
}

make_bundle() {
  id=$1 cand=$2 bundle=$3
  : > "$bundle"
  while IFS="$(printf '\t')" read -r tid _rest; do
    [ -n "$tid" ] || continue
    if [ "$tid" = "$id" ]; then cat "$cand" >> "$bundle"
    else cat "$REF/$tid.f" >> "$bundle"; fi
    printf '\n' >> "$bundle"
  done < "$TASK_LINES"
  cat bench/llm/tests.f >> "$bundle"
}

run_tests() {
  id=$1 cand=$2
  bundle=$T/test-$id.f
  err=$T/test-$id.err
  make_bundle "$id" "$cand" "$bundle"
  out=$(bin/hb < "$bundle" 2>"$err" || true)
  [ "$out" = ok ]
}

check_candidate() {
  cand=$1 diag=$2
  if $CHECK --json-errors --all-errors "$cand" </dev/null >/dev/null 2>"$diag"; then
    return 0
  fi
  return 1
}

field_ok() {
  field=$1 diag=$2 count=$3
  [ "$count" -eq 0 ] && { printf 1; return; }
  if grep -q "\"$field\":" "$diag"; then printf 1; else printf 0; fi
}

stable_diag() {
  cand=$1 diag=$2 count=$3
  [ "$count" -eq 0 ] && { printf 1; return; }
  again=$T/stable.err
  check_candidate "$cand" "$again" && { printf 0; return; }
  if cmp -s "$diag" "$again"; then printf 1; else printf 0; fi
}

repair_class_order() {
  printf '%s\n' \
    remove_producer \
    add_producer \
    fix_type \
    fix_return_stack \
    trusted_boundary_required \
    fix_signature_syntax \
    rewrite_uncheckable \
    unknown_rejection
}

diag_classes() {
  sed -n 's/.*"repair_class":"\([^"]*\)".*/\1/p' "$1"
}

emit_class_stat() {
  cls=$1 events=$2 success=$3 token_delta=$4
  diag_count=$(awk -F '\t' -v c="$cls" '$2 == c { n++ } END { print n + 0 }' "$events")
  [ "$diag_count" -gt 0 ] || return 0
  round_count=$(awk -F '\t' -v c="$cls" '$2 == c { seen[$1] = 1 } END { for (k in seen) n++; print n + 0 }' "$events")
  [ "$success" = 1 ] && class_success=true || class_success=false
  [ "$CLASS_STAT_FIRST" = 1 ] || printf ','
  CLASS_STAT_FIRST=0
  printf '{"repair_class":"%s","diagnostic_count":%s,"repair_success":%s,"repair_iterations":%s,"token_delta":%s}' \
    "$(json_escape "$cls")" "$diag_count" "$class_success" "$round_count" "$token_delta"
}

emit_repair_class_stats() {
  events=$1 success=$2 token_delta=$3
  printf '['
  if [ -s "$events" ]; then
    CLASS_STAT_FIRST=1
    known=
    for cls in $(repair_class_order); do
      known="$known $cls"
      emit_class_stat "$cls" "$events" "$success" "$token_delta"
    done
    for cls in $(awk -F '\t' '{ print $2 }' "$events" | sort -u); do
      case " $known " in
        *" $cls "*) continue ;;
      esac
      emit_class_stat "$cls" "$events" "$success" "$token_delta"
    done
  fi
  printf ']'
}

row_for_task() {
  id=$1 name=$2 sig=$3
  exp_sig=$(norm_sig "$sig")
  diag_all=$T/diag-$id.all
  class_events=$T/classes-$id.tsv
  : > "$diag_all"
  : > "$class_events"
  first_checker=rejected
  first_tests=0
  tests=0
  checkers=0
  repairs=0
  final=
  first_bad=
  start=$(date +%s)

  for cand in $(candidates_for "$id"); do
    checkers=$((checkers + 1))
    diag=$T/diag-$id-$checkers.err
    if check_candidate "$cand" "$diag"; then
      [ "$checkers" -eq 1 ] && first_checker=certified
      if run_tests "$id" "$cand"; then
        [ "$checkers" -eq 1 ] && first_tests=1
        tests=1
        final=$cand
        break
      else
        [ "$checkers" -eq 1 ] && first_tests=0
        final=$cand
      fi
    else
      [ "$checkers" -eq 1 ] && first_bad=$cand
      cat "$diag" >> "$diag_all"
      diag_classes "$diag" | while IFS= read -r cls; do
        [ -n "$cls" ] || continue
        printf '%s\t%s\n' "$checkers" "$cls" >> "$class_events"
      done
      final=$cand
    fi
  done

  [ -n "$final" ] || { echo "run-attempts: no final candidate for task $id" >&2; exit 66; }
  repairs=$((checkers - 1))
  dcount=$(wc -l < "$diag_all" | tr -d ' ')
  dtok=$(field_ok token "$diag_all" "$dcount")
  dspan=$(field_ok byte_start "$diag_all" "$dcount")
  dexp=$(field_ok expected "$diag_all" "$dcount")
  dact=$(field_ok actual "$diag_all" "$dcount")
  dcode=$(field_ok code "$diag_all" "$dcount")
  dclass=$(field_ok repair_class "$diag_all" "$dcount")
  if [ -n "$first_bad" ]; then ae=$(stable_diag "$first_bad" "$diag_all" "$dcount"); else ae=1; fi
  chars=$(wc -c < "$final" | tr -d ' ')
  trust=$(grep -Eic '(^|[^A-Za-z0-9_-])(TRUST|trust)([^A-Za-z0-9_-]|$)' "$final" || true)
  fsig=$(file_sig "$final")
  sigweak=0; [ "$(norm_sig "$fsig")" = "$exp_sig" ] || sigweak=1
  wall=$(( ( $(date +%s) - start ) * 1000 ))

  printf '{"schema_version":1,"run_id":"%s","task_id":%s,"name":"%s","model":"%s","attempt":%s,' \
    "$(json_escape "$RUN_ID")" "$id" "$(json_escape "$name")" "$(json_escape "$MODEL")" "$checkers" >> "$OUT"
  printf '"first_pass_checker":"%s","first_pass_tests":%s,"tests_passed":%s,' \
    "$first_checker" "$(bool "$first_tests")" "$(bool "$tests")" >> "$OUT"
  printf '"repair_iterations":%s,"checker_iterations":%s,"diagnostic_count":%s,' \
    "$repairs" "$checkers" "$dcount" >> "$OUT"
  printf '"diagnostic_token":%s,"diagnostic_span":%s,"diagnostic_expected":%s,"diagnostic_actual":%s,' \
    "$(bool "$dtok")" "$(bool "$dspan")" "$(bool "$dexp")" "$(bool "$dact")" >> "$OUT"
  printf '"diagnostic_code":%s,"diagnostic_repair_class":%s,"all_errors_stable":%s,' \
    "$(bool "$dcode")" "$(bool "$dclass")" "$(bool "$ae")" >> "$OUT"
  printf '"repair_class_stats":' >> "$OUT"
  emit_repair_class_stats "$class_events" "$tests" 0 >> "$OUT"
  printf ',' >> "$OUT"
  printf '"tokens_used":0,"wall_ms":%s,"final_chars":%s,"trust_uses":%s,"signature_weakened":%s}\n' \
    "$wall" "$chars" "$trust" "$(bool "$sigweak")" >> "$OUT"
}

while IFS="$(printf '\t')" read -r id name sig _cat _tests; do
  [ -n "$id" ] || continue
  row_for_task "$id" "$name" "$sig"
done < "$TASK_LINES"

VALIDATOR=$T/validate-results.f
cat tools/date.f tools/lint/lib.f tools/json.f tools/argv.f bench/llm/validate-results.f > "$VALIDATOR"
bin/hb "$VALIDATOR" "$OUT" >&2
echo "run-attempts: wrote $OUT" >&2
