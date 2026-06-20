#!/bin/sh
# drive-forth.sh — live model driver for one harness=forth manifest row.
# The model writes one checked Forth word. We certify the candidate, replace the
# reference definition for this task, run the full benchmark Forth test corpus,
# and emit one schema-v2 live metrics row with replay artifacts.
# Usage: drive-forth.sh <id> <name> <sig> <category> <tests> <spec> [maxr]
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
CHECK="bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f tools/argv.f tools/check.f --"

ID=$1
NAME=$2
SIG=$3
CATEGORY=$4
TESTS=$5
SPEC=$6
MAXR=${7:-5}
FEEDBACK_MODE=${BENCH_FORTH_FEEDBACK:-repair}
case "$FEEDBACK_MODE" in
  repair) DEFAULT_ARM=habu-forth ;;
  raw) DEFAULT_ARM=habu-forth-raw ;;
  blind) DEFAULT_ARM=habu-forth-blind ;;
  *) echo "drive-forth: unknown feedback mode $FEEDBACK_MODE" >&2; exit 64 ;;
esac
ARM=${BENCH_FORTH_ARM:-$DEFAULT_ARM}

model_init

T=$(mktemp -d "${TMPDIR:-/tmp}/df.XXXXXX")
trap 'rm -rf "$T"' EXIT HUP INT TERM

REF=$T/ref
TASK_LINES=$T/tasks.body
mkdir -p "$REF"
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
      print "drive-forth: solution without harness=forth task: " name > "/dev/stderr"
      exit 1
    }
    print > (dir "/" task_id[name] ".f")
  }
' bench/llm/tasks.tsv bench/llm/solutions.f

[ -f "$REF/$ID.f" ] || { echo "drive-forth: no reference solution for task $ID" >&2; exit 66; }

printf 'prompt unavailable\n' > "$T/prompt.txt"
printf 'response unavailable\n' > "$T/resp.json"
printf '\\ no candidate extracted\n' > "$T/cand.f"
printf '\\ no final bundle\n' > "$T/bundle.f"
: > "$T/checker-diagnostics.txt"
: > "$T/repair-packet.json"
: > "$T/checker-stdout.txt"
: > "$T/checker-prose.txt"
: > "$T/test-output.txt"
: > "$T/class-events.tsv"
: > "$T/last-message.txt"

REPAIR_TOOL=$T/repair-packet-tool.f
cat tools/argv.f tools/json.f tools/repair-packet.f > "$REPAIR_TOOL"

norm_sig() {
  printf '%s' "$1" | sed 's/^[(]//; s/[)]$//' | xargs
}

file_sig() {
  awk '/^:/ {
    s = index($0, "("); e = index($0, ")");
    if (s > 0 && e > s) { print substr($0, s + 1, e - s - 1); exit }
  }' "$1" | xargs
}

extract() {
  sed 's/^```.*$//' "$1" | awk '
    /^[[:space:]]*:/ { s = 1 }
    s { print; if ($0 ~ /;/) exit }
  '
}

candidate_name() {
  awk '/^[[:space:]]*:/ { print $2; exit }' "$1"
}

candidate_forbidden_boundary() {
  grep -Eiq '(^|[^A-Za-z0-9_-])(trust|set-check)([^A-Za-z0-9_-]|$)' "$1"
}

make_bundle() {
  cand=$1
  bundle=$2
  : > "$bundle"
  while IFS="$(printf '\t')" read -r tid _rest; do
    [ -n "$tid" ] || continue
    if [ "$tid" = "$ID" ]; then
      cat "$cand" >> "$bundle"
    else
      cat "$REF/$tid.f" >> "$bundle"
    fi
    printf '\n' >> "$bundle"
  done < "$TASK_LINES"
  cat bench/llm/tests.f >> "$bundle"
}

check_candidate() {
  cand=$1
  diag=$2
  if $CHECK --json-errors --all-errors "$cand" </dev/null >"$T/checker-stdout.txt" 2>"$diag"; then
    return 0
  fi
  if [ ! -s "$diag" ] && [ -s "$T/checker-stdout.txt" ]; then
    cp "$T/checker-stdout.txt" "$diag"
  fi
  if ! grep -q '^[[:space:]]*{' "$diag"; then
    raw=$(tr '\n' ' ' < "$diag" | xargs)
    [ -n "$raw" ] || raw=checker_failed_without_diagnostic
    token=$(printf '%s' "$raw" | awk '{ print $1 }')
    [ -n "$token" ] || token=unknown
    suggestion="Replace unsupported or undefined tokens with checked Habu words."
    if [ "$token" = "$NAME" ]; then
      suggestion="Use recurse for recursive calls inside the current definition."
    fi
    definition_source=$(bench_json_quote_file "$cand")
    printf '{' > "$diag"
    printf '"schema_version":1,"verdict":"rejected","word":%s,' "$(bench_json_quote "$NAME")" >> "$diag"
    printf '"token":%s,"token_index":0,"file":%s,' "$(bench_json_quote "$token")" "$(bench_json_quote "$cand")" >> "$diag"
    printf '"line":1,"column":1,"byte_start":0,"byte_end":0,' >> "$diag"
    printf '"declared_effect":%s,"inferred_effect":%s,' "$(bench_json_quote "$SIG")" "$(bench_json_quote "$raw")" >> "$diag"
    printf '"expected":null,"actual":null,"return_stack":{"expected":null,"actual":null},' >> "$diag"
    printf '"code":"E-UNSTRUCTURED-CHECK","repair_class":"unknown_rejection",' >> "$diag"
    printf '"suggestion":%s,"definition_source":%s}\n' "$(bench_json_quote "$suggestion")" "$definition_source" >> "$diag"
  fi
  return 1
}

run_tests() {
  make_bundle "$1" "$T/bundle.f"
  set +e
  out=$(timeout 10 bin/hb < "$T/bundle.f" 2>"$T/test-stderr.txt")
  rc=$?
  set -e
  {
    printf 'exit_code=%s\n' "$rc"
    printf '%s\n' "$out"
    cat "$T/test-stderr.txt"
  } > "$T/test-output.txt"
  [ "$rc" -eq 0 ] && [ "$out" = ok ]
}

field_ok() {
  field=$1
  diag=$2
  count=$3
  [ "$count" -eq 0 ] && { printf true; return; }
  if grep -q "\"$field\":" "$diag"; then printf true; else printf false; fi
}

stable_diag() {
  cand=$1
  diag=$2
  count=$3
  [ "$count" -eq 0 ] && { printf true; return; }
  again=$T/stable.err
  check_candidate "$cand" "$again" && { printf false; return; }
  if cmp -s "$diag" "$again"; then printf true; else printf false; fi
}

diag_classes() {
  sed -n 's/.*"repair_class":"\([^"]*\)".*/\1/p' "$1"
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

emit_one_class_stat() {
  cls=$1
  events=$2
  success=$3
  token_delta=$4
  diag_count=$(awk -F '\t' -v c="$cls" '$2 == c { n++ } END { print n + 0 }' "$events")
  [ "$diag_count" -gt 0 ] || return 0
  round_count=$(awk -F '\t' -v c="$cls" '$2 == c { seen[$1] = 1 } END { for (k in seen) n++; print n + 0 }' "$events")
  [ "$CLASS_FIRST" = 1 ] || printf ','
  CLASS_FIRST=0
  printf '{"repair_class":%s,"diagnostic_count":%s,"repair_success":%s,"repair_iterations":%s,"token_delta":%s}' \
    "$(bench_json_quote "$cls")" "$diag_count" "$success" "$round_count" "$token_delta"
}

repair_class_stats_json() {
  events=$1
  success=$2
  token_delta=$3
  printf '['
  if [ -s "$events" ]; then
    CLASS_FIRST=1
    known=
    for cls in $(repair_class_order); do
      known="$known $cls"
      emit_one_class_stat "$cls" "$events" "$success" "$token_delta"
    done
    for cls in $(awk -F '\t' '{ print $2 }' "$events" | sort -u); do
      case " $known " in
        *" $cls "*) continue ;;
      esac
      emit_one_class_stat "$cls" "$events" "$success" "$token_delta"
    done
  fi
  printf ']'
}

checker_feedback() {
  cand=$1
  diag=$2
  packet=$3
  case "$FEEDBACK_MODE" in
    repair)
      printf '\n\nYour attempt:\n'
      cat "$cand"
      printf '\nThe checker rejected it. Use this repair packet:\n'
      cat "$packet"
      printf '\n\nFix the body so it satisfies the declared stack effect. Output only the corrected definition.'
      ;;
    raw)
      printf '\n\nYour attempt:\n'
      cat "$cand"
      printf '\nThe checker rejected it. Raw checker diagnostics:\n'
      cat "$diag"
      printf '\n\nFix the body so it satisfies the declared stack effect. Output only the corrected definition.'
      ;;
    blind)
      printf '\n\nYour attempt did not certify against the declared stack effect. Fix the body. Output only the corrected definition.'
      ;;
  esac
}

test_feedback() {
  cand=$1
  test_out=$2
  case "$FEEDBACK_MODE" in
    repair|raw)
      printf '\n\nYour attempt:\n'
      cat "$cand"
      printf '\nIt certified, but failed the benchmark tests. Test output:\n'
      cat "$test_out"
      printf '\n\nExpected examples:\n%s\n\nFix the logic. Output only the corrected definition.' "$cases"
      ;;
    blind)
      printf '\n\nYour attempt certified, but failed the benchmark tests. Fix the logic. Output only the corrected definition.'
      ;;
  esac
}

codex_output_tokens() {
  sed -n 's/.*"turn.completed".*"output_tokens":\([0-9][0-9]*\).*/\1/p' "$1" | tail -n 1
}

parse_model_response() {
  resp=$1
  text=$2
  last=$3
  if [ -s "$last" ]; then
    cp "$last" "$text"
    rt=$(codex_output_tokens "$resp")
    [ -n "$rt" ] || rt=0
    printf '%s' "$rt"
    return 0
  fi
  if rt=$(bin/hb --load lib/errors.f lib/string.f lib/fs.f tools/json.f tools/argv.f bench/llm/parse-resp-lib.f bench/llm/parse-resp.f -- "$resp" "$text" "$MODEL_PARSER" "$MODEL_TOKEN_FIELDS"); then
    printf '%s' "$rt"
    return 0
  fi
  printf 'response parse failed\n' > "$text"
  printf '0'
}

cases=$(printf '%s' "$TESTS" | tr ';' '\n' | sed '/^[[:space:]]*$/d')
TASK="Define exactly one checked Habu Forth word:
  : ${NAME} ( ${SIG} ) ... ;

${SPEC}

Expected examples:
${cases}

Rules:
- Output only the definition, no markdown or prose.
- Keep the word name and stack effect exactly as shown.
- Use checked Forth; do not use TRUST, trust, or 0 set-check.
- Project words are UPPER-CASE; built-in words stay lower-case."

round=0
feedback=
outcome=reject
toks=0
t0=$(now_ms)
first_checker=rejected
first_tests=false
tests_passed=false
first_bad=
final_cand=$T/cand.f

while [ "$round" -lt "$MAXR" ]; do
  round=$((round + 1))
  prompt="${TASK}${feedback}"
  printf '%s' "$prompt" > "$T/prompt.txt"
  : > "$T/last-message.txt"
  MODEL_LAST_MESSAGE_FILE=$T/last-message.txt
  if ! model_run "$prompt" "$T/resp.json"; then
    printf 'model_run_failed\n' > "$T/resp.json"
    outcome=error
    break
  fi
  rt=$(parse_model_response "$T/resp.json" "$T/text.txt" "$T/last-message.txt")
  toks=$((toks + rt))
  extract "$T/text.txt" > "$T/cand.f"
  [ -s "$T/cand.f" ] || printf '\\ no candidate extracted\n' > "$T/cand.f"
  final_cand=$T/cand.f
  if ! grep -q ';' "$T/cand.f"; then
    feedback="

No complete Forth definition was extracted. Output exactly:
: ${NAME} ( ${SIG} ) ... ;
with no prose."
    outcome=reject
    continue
  fi
  cname=$(candidate_name "$T/cand.f")
  if [ "$cname" != "$NAME" ]; then
    feedback="

The definition name must be exactly ${NAME}; got ${cname:-nothing}. Output exactly one checked definition:
: ${NAME} ( ${SIG} ) ... ;"
    outcome=reject
    continue
  fi
  if candidate_forbidden_boundary "$T/cand.f"; then
    feedback="

The candidate used an unchecked/trusted boundary. This benchmark requires checked Forth only: no TRUST, trust, or set-check. Rewrite the body so the checker can verify it."
    outcome=reject
    continue
  fi
  if check_candidate "$T/cand.f" "$T/round-$round.err"; then
    [ "$round" -eq 1 ] && first_checker=certified
    if run_tests "$T/cand.f"; then
      [ "$round" -eq 1 ] && first_tests=true
      tests_passed=true
      outcome=pass
      break
    fi
    [ "$round" -eq 1 ] && first_tests=false
    outcome=fail
    feedback=$(test_feedback "$T/cand.f" "$T/test-output.txt")
  else
    [ "$round" -eq 1 ] && first_bad=$T/first-bad.f && cp "$T/cand.f" "$first_bad"
    [ "$round" -eq 1 ] && first_checker=rejected
    cat "$T/round-$round.err" >> "$T/checker-diagnostics.txt"
    diag_classes "$T/round-$round.err" | while IFS= read -r cls; do
      [ -n "$cls" ] || continue
      printf '%s\t%s\n' "$round" "$cls" >> "$T/class-events.tsv"
    done
    if ! bin/hb "$REPAIR_TOOL" "$T/round-$round.err" > "$T/repair-packet.json"; then
      outcome=error
      break
    fi
    cp "$T/repair-packet.json" "$T/test-output.txt"
    outcome=reject
    feedback=$(checker_feedback "$T/cand.f" "$T/round-$round.err" "$T/repair-packet.json")
  fi
done

make_bundle "$final_cand" "$T/bundle.f"
wall=$(( $(now_ms) - t0 ))
diagnostic_count=$(wc -l < "$T/checker-diagnostics.txt" | tr -d ' ')
[ -n "$first_bad" ] || first_bad=$final_cand
fsig=$(file_sig "$final_cand")
sigweak=false
[ "$(norm_sig "$fsig")" = "$(norm_sig "$SIG")" ] || sigweak=true
trust=$(grep -Eic '(^|[^A-Za-z0-9_-])(TRUST|trust)([^A-Za-z0-9_-]|$)' "$final_cand" || true)
class_success=false
[ "$tests_passed" = true ] && class_success=true

BENCH_TASK_FAMILY=${BENCH_TASK_FAMILY:-$CATEGORY}
BENCH_PROMPT_FILE=$T/prompt.txt
BENCH_RAW_RESPONSE_FILE=$T/resp.json
BENCH_CANDIDATE_FILE=$T/cand.f
BENCH_CHECKER_DIAGNOSTICS_FILE=$T/checker-diagnostics.txt
BENCH_REPAIR_PACKET_FILE=$T/repair-packet.json
BENCH_TEST_OUTPUT_FILE=$T/test-output.txt
BENCH_FINAL_BUNDLE_FILE=$T/bundle.f
BENCH_SOURCE_FILE=$final_cand
BENCH_FIRST_PASS_CHECKER=$first_checker
BENCH_FIRST_PASS_TESTS=$first_tests
BENCH_TESTS_PASSED=$tests_passed
BENCH_CHECKER_ITERATIONS=$round
BENCH_REPAIR_ITERATIONS=0
[ "$round" -gt 0 ] && BENCH_REPAIR_ITERATIONS=$((round - 1))
BENCH_DIAGNOSTIC_COUNT=$diagnostic_count
BENCH_DIAGNOSTIC_TOKEN=$(field_ok token "$T/checker-diagnostics.txt" "$diagnostic_count")
BENCH_DIAGNOSTIC_SPAN=$(field_ok byte_start "$T/checker-diagnostics.txt" "$diagnostic_count")
BENCH_DIAGNOSTIC_EXPECTED=$(field_ok expected "$T/checker-diagnostics.txt" "$diagnostic_count")
BENCH_DIAGNOSTIC_ACTUAL=$(field_ok actual "$T/checker-diagnostics.txt" "$diagnostic_count")
BENCH_DIAGNOSTIC_CODE=$(field_ok code "$T/checker-diagnostics.txt" "$diagnostic_count")
BENCH_DIAGNOSTIC_REPAIR_CLASS=$(field_ok repair_class "$T/checker-diagnostics.txt" "$diagnostic_count")
BENCH_ALL_ERRORS_STABLE=$(stable_diag "$first_bad" "$T/checker-diagnostics.txt" "$diagnostic_count")
BENCH_REPAIR_CLASS_STATS=$(repair_class_stats_json "$T/class-events.tsv" "$class_success" 0)
BENCH_TRUST_USES=$trust
BENCH_SIGNATURE_WEAKENED=$sigweak

emit_row "$ID" "$NAME" "$MODEL" "$ARM" "$outcome" "$round" "$toks" "$wall" null not_run
