#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."
ROOT=$(pwd)
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-llm-results.XXXXXX")
HASH=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
cleanup() {
  if command -v trash >/dev/null 2>&1; then
    trash "$T"
  else
    rm -r "$T"
  fi
}
trap cleanup EXIT HUP INT TERM

BUNDLE=$T/validate-results.f
cat "$ROOT/tools/date.f" "$ROOT/tools/lint/lib.f" "$ROOT/tools/json.f" "$ROOT/tools/argv.f" "$ROOT/bench/llm/validate-results.f" > "$BUNDLE"

mkdir -p "$T/bench/llm/results"
cp "$ROOT/bench/llm/tasks.tsv" "$T/bench/llm/tasks.tsv"

write_reference_jsonl() {
  awk -F '\t' '
    NR > 1 && $6 == "forth" {
      printf "{\"schema_version\":1,\"run_id\":\"reference-2026-06-18\",\"task_id\":%s,\"name\":\"%s\",\"model\":\"reference\",\"attempt\":1,", $1, $2
      printf "\"first_pass_checker\":\"certified\",\"first_pass_tests\":true,\"tests_passed\":true,"
      printf "\"repair_iterations\":0,\"checker_iterations\":1,\"diagnostic_count\":0,"
      printf "\"diagnostic_token\":true,\"diagnostic_span\":true,\"diagnostic_expected\":true,"
      printf "\"diagnostic_actual\":true,\"diagnostic_code\":true,\"diagnostic_repair_class\":true,"
      printf "\"all_errors_stable\":true,\"tokens_used\":0,\"wall_ms\":0,\"final_chars\":1,"
      printf "\"trust_uses\":0,\"signature_weakened\":false}\n"
    }
  ' "$T/bench/llm/tasks.tsv" > "$T/bench/llm/results/reference.jsonl"
}

write_live_jsonl() {
  trials=$1
  awk -F '\t' -v trials="$trials" -v hash="$HASH" '
    NR > 1 {
      for (trial = 1; trial <= trials; trial++) {
        printf "{\"schema_version\":2,\"run_id\":\"live-fixture-2026-06-18\","
        printf "\"model_id\":\"toy-model\",\"arm\":\"forth\",\"trial_id\":\"live-fixture-2026-06-18:toy-model:forth:%s:%d\",", $1, trial
        printf "\"task_family\":\"%s\",\"model_version\":\"unknown\",\"model_date\":\"unknown\",", $4
        printf "\"trial\":%d,\"task_order\":%s,\"k_trials\":%d,\"order_seed\":\"live-fixture\",", trial, $1, trials
        printf "\"task_id\":%s,\"name\":\"%s\",\"model\":\"toy-model\",\"attempt\":%d,", $1, $2, trial
        printf "\"first_pass_checker\":\"certified\",\"first_pass_tests\":true,\"tests_passed\":true,"
        printf "\"repair_iterations\":0,\"checker_iterations\":1,\"diagnostic_count\":0,"
        printf "\"diagnostic_token\":true,\"diagnostic_span\":true,\"diagnostic_expected\":true,"
        printf "\"diagnostic_actual\":true,\"diagnostic_code\":true,\"diagnostic_repair_class\":true,"
        printf "\"all_errors_stable\":true,\"tokens_used\":0,\"wall_ms\":0,\"final_chars\":1,"
        printf "\"trust_uses\":0,\"signature_weakened\":false,"
        printf "\"outcome\":\"pass\",\"rounds\":1,\"first_pass\":true,\"tokens\":0,\"source_chars\":1,"
        printf "\"runtime_ms\":null,\"runtime_repetitions\":100,\"runtime_warmups\":10,\"runtime_status\":\"not_run\","
        printf "\"prompt\":\"prompt\",\"prompt_sha256\":\"%s\",", hash
        printf "\"raw_response\":\"raw\",\"raw_response_sha256\":\"%s\",", hash
        printf "\"extracted_candidate\":\"candidate\",\"extracted_candidate_sha256\":\"%s\",", hash
        printf "\"checker_diagnostics\":\"\",\"checker_diagnostics_sha256\":\"%s\",", hash
        printf "\"repair_packet\":\"\",\"repair_packet_sha256\":\"%s\",", hash
        printf "\"test_output\":\"ok\",\"test_output_sha256\":\"%s\",", hash
        printf "\"final_bundle\":\"bundle\",\"final_bundle_sha256\":\"%s\"}\n", hash
      }
    }
  ' "$T/bench/llm/tasks.tsv" > "$T/bench/llm/results/live.jsonl"
}

write_arm_jsonl() {
  : > "$T/bench/llm/results/live.jsonl"
  for arm in habu-forth habu-forth-raw habu-forth-blind; do
    printf '{"schema_version":2,"run_id":"arm-fixture-2026-06-18",' >> "$T/bench/llm/results/live.jsonl"
    printf '"model_id":"toy-model","arm":"%s","trial_id":"arm-fixture-2026-06-18:toy-model:%s:1:1",' "$arm" "$arm" >> "$T/bench/llm/results/live.jsonl"
    printf '"task_family":"arithmetic","model_version":"unknown","model_date":"unknown",' >> "$T/bench/llm/results/live.jsonl"
    printf '"trial":1,"task_order":1,"k_trials":1,"order_seed":"arm-fixture",' >> "$T/bench/llm/results/live.jsonl"
    printf '"task_id":1,"name":"SQUARE","model":"toy-model","attempt":1,' >> "$T/bench/llm/results/live.jsonl"
    printf '"first_pass_checker":"certified","first_pass_tests":true,"tests_passed":true,' >> "$T/bench/llm/results/live.jsonl"
    printf '"repair_iterations":0,"checker_iterations":1,"diagnostic_count":0,' >> "$T/bench/llm/results/live.jsonl"
    printf '"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,' >> "$T/bench/llm/results/live.jsonl"
    printf '"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,' >> "$T/bench/llm/results/live.jsonl"
    printf '"all_errors_stable":true,"tokens_used":0,"wall_ms":0,"final_chars":1,' >> "$T/bench/llm/results/live.jsonl"
    printf '"trust_uses":0,"signature_weakened":false,' >> "$T/bench/llm/results/live.jsonl"
    printf '"outcome":"pass","rounds":1,"first_pass":true,"tokens":0,"source_chars":1,' >> "$T/bench/llm/results/live.jsonl"
    printf '"runtime_ms":null,"runtime_repetitions":100,"runtime_warmups":10,"runtime_status":"not_run",' >> "$T/bench/llm/results/live.jsonl"
    printf '"prompt":"prompt","prompt_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"raw_response":"raw","raw_response_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"extracted_candidate":"candidate","extracted_candidate_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"checker_diagnostics":"","checker_diagnostics_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"repair_packet":"","repair_packet_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"test_output":"ok","test_output_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"final_bundle":"bundle","final_bundle_sha256":"%s"}\n' "$HASH" >> "$T/bench/llm/results/live.jsonl"
  done
}

write_multi_model_jsonl() {
  : > "$T/bench/llm/results/live.jsonl"
  for model_id in alpha beta; do
    case "$model_id" in
      alpha) label=Alpha ;;
      beta) label=Beta ;;
    esac
    printf '{"schema_version":2,"run_id":"multi-model-fixture-2026-06-18",' >> "$T/bench/llm/results/live.jsonl"
    printf '"model_id":"%s","arm":"forth","trial_id":"multi-model-fixture-2026-06-18:%s:forth:1:1",' "$model_id" "$model_id" >> "$T/bench/llm/results/live.jsonl"
    printf '"task_family":"arithmetic","model_version":"unknown","model_date":"unknown",' >> "$T/bench/llm/results/live.jsonl"
    printf '"trial":1,"task_order":1,"k_trials":1,"order_seed":"multi-model-fixture",' >> "$T/bench/llm/results/live.jsonl"
    printf '"task_id":1,"name":"SQUARE","model":"%s","attempt":1,' "$label" >> "$T/bench/llm/results/live.jsonl"
    printf '"first_pass_checker":"certified","first_pass_tests":true,"tests_passed":true,' >> "$T/bench/llm/results/live.jsonl"
    printf '"repair_iterations":0,"checker_iterations":1,"diagnostic_count":0,' >> "$T/bench/llm/results/live.jsonl"
    printf '"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,' >> "$T/bench/llm/results/live.jsonl"
    printf '"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,' >> "$T/bench/llm/results/live.jsonl"
    printf '"all_errors_stable":true,"tokens_used":0,"wall_ms":0,"final_chars":1,' >> "$T/bench/llm/results/live.jsonl"
    printf '"trust_uses":0,"signature_weakened":false,' >> "$T/bench/llm/results/live.jsonl"
    printf '"outcome":"pass","rounds":1,"first_pass":true,"tokens":0,"source_chars":1,' >> "$T/bench/llm/results/live.jsonl"
    printf '"runtime_ms":null,"runtime_repetitions":100,"runtime_warmups":10,"runtime_status":"not_run",' >> "$T/bench/llm/results/live.jsonl"
    printf '"prompt":"prompt","prompt_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"raw_response":"raw","raw_response_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"extracted_candidate":"candidate","extracted_candidate_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"checker_diagnostics":"","checker_diagnostics_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"repair_packet":"","repair_packet_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"test_output":"ok","test_output_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"final_bundle":"bundle","final_bundle_sha256":"%s"}\n' "$HASH" >> "$T/bench/llm/results/live.jsonl"
  done
}

write_confidence_jsonl() {
  : > "$T/bench/llm/results/live.jsonl"
  for trial in 1 2; do
    if [ "$trial" = 1 ]; then
      checker=certified
      first_tests=true
      tests=true
      outcome=pass
    else
      checker=rejected
      first_tests=false
      tests=false
      outcome=fail
    fi
    printf '{"schema_version":2,"run_id":"confidence-fixture-2026-06-18",' >> "$T/bench/llm/results/live.jsonl"
    printf '"model_id":"toy-model","arm":"forth","trial_id":"confidence-fixture-2026-06-18:toy-model:forth:1:%s",' "$trial" >> "$T/bench/llm/results/live.jsonl"
    printf '"task_family":"arithmetic","model_version":"unknown","model_date":"unknown",' >> "$T/bench/llm/results/live.jsonl"
    printf '"trial":%s,"task_order":1,"k_trials":2,"order_seed":"confidence-fixture",' "$trial" >> "$T/bench/llm/results/live.jsonl"
    printf '"task_id":1,"name":"SQUARE","model":"toy-model","attempt":%s,' "$trial" >> "$T/bench/llm/results/live.jsonl"
    printf '"first_pass_checker":"%s","first_pass_tests":%s,"tests_passed":%s,' "$checker" "$first_tests" "$tests" >> "$T/bench/llm/results/live.jsonl"
    printf '"repair_iterations":0,"checker_iterations":1,"diagnostic_count":0,' >> "$T/bench/llm/results/live.jsonl"
    printf '"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,' >> "$T/bench/llm/results/live.jsonl"
    printf '"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,' >> "$T/bench/llm/results/live.jsonl"
    printf '"all_errors_stable":true,"tokens_used":0,"wall_ms":0,"final_chars":1,' >> "$T/bench/llm/results/live.jsonl"
    printf '"trust_uses":0,"signature_weakened":false,' >> "$T/bench/llm/results/live.jsonl"
    printf '"outcome":"%s","rounds":1,"first_pass":%s,"tokens":0,"source_chars":1,' "$outcome" "$tests" >> "$T/bench/llm/results/live.jsonl"
    printf '"runtime_ms":null,"runtime_repetitions":100,"runtime_warmups":10,"runtime_status":"not_run",' >> "$T/bench/llm/results/live.jsonl"
    printf '"prompt":"prompt","prompt_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"raw_response":"raw","raw_response_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"extracted_candidate":"candidate","extracted_candidate_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"checker_diagnostics":"","checker_diagnostics_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"repair_packet":"","repair_packet_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"test_output":"ok","test_output_sha256":"%s",' "$HASH" >> "$T/bench/llm/results/live.jsonl"
    printf '"final_bundle":"bundle","final_bundle_sha256":"%s"}\n' "$HASH" >> "$T/bench/llm/results/live.jsonl"
  done
}

write_reference_jsonl

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE")
expected_count=$(awk -F '\t' 'NR>1 && $6 == "forth" {n++} END{print n+0}' "$ROOT/bench/llm/tasks.tsv")
expected_all_count=$(awk -F '\t' 'NR>1 {n++} END{print n+0}' "$ROOT/bench/llm/tasks.tsv")
expected="llm-results: $expected_count reference metric row(s), 0 finding(s)"
[ "$out" = "$expected" ] || {
  echo "FAIL: validate-results good fixture: $out"
  exit 1
}

awk '
{
  gsub(/"run_id":"[^"]+"/, "\"run_id\":\"attempt-fixture\"");
  gsub(/"model":"reference"/, "\"model\":\"toy-model\"");
  if ($0 ~ /"task_id":1,/) {
    gsub(/"first_pass_checker":"certified"/, "\"first_pass_checker\":\"rejected\"");
    gsub(/"first_pass_tests":true/, "\"first_pass_tests\":false");
    gsub(/"tests_passed":true/, "\"tests_passed\":false");
    gsub(/"repair_iterations":0/, "\"repair_iterations\":2");
    gsub(/"checker_iterations":1/, "\"checker_iterations\":3");
    gsub(/"diagnostic_count":0/, "\"diagnostic_count\":4");
    gsub(/"diagnostic_token":true/, "\"diagnostic_token\":false");
    gsub(/"diagnostic_span":true/, "\"diagnostic_span\":false");
    gsub(/"diagnostic_expected":true/, "\"diagnostic_expected\":false");
    gsub(/"diagnostic_actual":true/, "\"diagnostic_actual\":false");
    gsub(/"diagnostic_code":true/, "\"diagnostic_code\":false");
    gsub(/"diagnostic_repair_class":true/, "\"diagnostic_repair_class\":false");
    gsub(/"all_errors_stable":true/, "\"all_errors_stable\":false");
    gsub(/"tokens_used":0/, "\"tokens_used\":100");
    gsub(/"wall_ms":0/, "\"wall_ms\":250");
    sub(/"tokens_used":100/,
      "\"repair_class_stats\":[{\"repair_class\":\"remove_producer\",\"diagnostic_count\":2,\"repair_success\":false,\"repair_iterations\":1,\"token_delta\":30},{\"repair_class\":\"add_producer\",\"diagnostic_count\":1,\"repair_success\":false,\"repair_iterations\":2,\"token_delta\":50},{\"repair_class\":\"fix_type\",\"diagnostic_count\":1,\"repair_success\":false,\"repair_iterations\":1,\"token_delta\":20}],\"tokens_used\":100");
  }
  if ($0 ~ /"task_id":2,/) {
    gsub(/"trust_uses":0/, "\"trust_uses\":1");
    gsub(/"signature_weakened":false/, "\"signature_weakened\":true");
  }
  print
}
' "$ROOT/bench/llm/results/reference.jsonl" > "$T/bench/llm/results/attempt.jsonl"

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/attempt.jsonl)
expected_good=$((expected_count - 1))
expected_checkers=$((expected_count + 2))

printf '%s\n' "$out" | grep -q "run=attempt-fixture model=toy-model rows=$expected_count certified=$expected_good first_tests=$expected_good tests=$expected_good repairs=2 checker_iterations=$expected_checkers diagnostics=4 tokens=100 wall_ms=250" || {
  echo "FAIL: validate-results summary totals"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'buckets checker_rejected=1 first_tests_failed=1 tests_failed=1 trust_used=1 signature_weakened=1' || {
  echo "FAIL: validate-results summary buckets"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q "diagnostic_quality token=$expected_good span=$expected_good expected=$expected_good actual=$expected_good code=$expected_good repair_class=$expected_good all_errors_stable=$expected_good" || {
  echo "FAIL: validate-results diagnostic quality"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'diagnostic_gaps token=1 span=1 expected=1 actual=1 code=1 repair_class=1 all_errors_stable=1' || {
  echo "FAIL: validate-results diagnostic gaps"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'category arithmetic rows=6 certified=5 tests=5' || {
  echo "FAIL: validate-results summary category"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'repair_class remove_producer rows=1 repair_success=0 repair_iterations=1 diagnostics=2 token_delta=30' || {
  echo "FAIL: validate-results repair class remove_producer"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'repair_class add_producer rows=1 repair_success=0 repair_iterations=2 diagnostics=1 token_delta=50' || {
  echo "FAIL: validate-results repair class add_producer"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'repair_class fix_type rows=1 repair_success=0 repair_iterations=1 diagnostics=1 token_delta=20' || {
  echo "FAIL: validate-results repair class fix_type"
  printf '%s\n' "$out"
  exit 1
}

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" --json bench/llm/results/attempt.jsonl)
printf '%s\n' "$out" | grep -q "\"rows\":$expected_count" || {
  echo "FAIL: validate-results json rows"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q '"checker_rejected":1' || {
  echo "FAIL: validate-results json buckets"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q "\"diagnostic_quality\":{\"token\":$expected_good,\"span\":$expected_good,\"expected\":$expected_good,\"actual\":$expected_good,\"code\":$expected_good,\"repair_class\":$expected_good,\"all_errors_stable\":$expected_good}" || {
  echo "FAIL: validate-results json diagnostic quality"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q '"diagnostic_gaps":{"token":1,"span":1,"expected":1,"actual":1,"code":1,"repair_class":1,"all_errors_stable":1}' || {
  echo "FAIL: validate-results json diagnostic gaps"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q '"category":"arithmetic","rows":6,"certified":5,"tests_passed":5' || {
  echo "FAIL: validate-results json category"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q '"repair_classes":\[{"repair_class":"remove_producer","rows":1,"repair_success":0,"repair_iterations":1,"diagnostic_count":2,"token_delta":30},{"repair_class":"add_producer","rows":1,"repair_success":0,"repair_iterations":2,"diagnostic_count":1,"token_delta":50},{"repair_class":"fix_type","rows":1,"repair_success":0,"repair_iterations":1,"diagnostic_count":1,"token_delta":20}\]' || {
  echo "FAIL: validate-results json repair classes"
  printf '%s\n' "$out"
  exit 1
}

write_live_jsonl 2
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl)
expected_live_rows=$((expected_all_count * 2))
printf '%s\n' "$out" | grep -q "run=live-fixture-2026-06-18 model=toy-model rows=$expected_live_rows certified=$expected_live_rows first_tests=$expected_live_rows tests=$expected_live_rows repairs=0 checker_iterations=$expected_live_rows diagnostics=0 tokens=0 wall_ms=0" || {
  echo "FAIL: validate-results V2 k-trial summary"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'category arithmetic rows=12 certified=12 tests=12' || {
   echo "FAIL: validate-results V2 category k-trial accounting"
   printf '%s\n' "$out"
   exit 1
}
printf '%s\n' "$out" | grep -q 'category arrays rows=30 certified=30 tests=30' || {
   echo "FAIL: validate-results V2 expanded category accounting"
   printf '%s\n' "$out"
   exit 1
}

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" --json bench/llm/results/live.jsonl)
printf '%s\n' "$out" | grep -q "\"schema_version\":2" || {
  echo "FAIL: validate-results V2 json schema"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q "\"rows\":$expected_live_rows" || {
  echo "FAIL: validate-results V2 json rows"
  printf '%s\n' "$out"
  exit 1
}

write_arm_jsonl
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl)
printf '%s\n' "$out" | grep -q 'arm habu-forth rows=1 certified=1 first_tests=1 tests=1 repairs=0 checker_iterations=1 diagnostics=0 tokens=0 wall_ms=0 final_chars=1' || {
  echo "FAIL: validate-results arm habu-forth summary"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'arm habu-forth-raw rows=1 certified=1 first_tests=1 tests=1 repairs=0 checker_iterations=1 diagnostics=0 tokens=0 wall_ms=0 final_chars=1' || {
  echo "FAIL: validate-results arm habu-forth-raw summary"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q 'arm habu-forth-blind rows=1 certified=1 first_tests=1 tests=1 repairs=0 checker_iterations=1 diagnostics=0 tokens=0 wall_ms=0 final_chars=1' || {
  echo "FAIL: validate-results arm habu-forth-blind summary"
  printf '%s\n' "$out"
  exit 1
}

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" --json bench/llm/results/live.jsonl)
printf '%s\n' "$out" | grep -q '"task_groups":3,"task_pass_at_k":3,"trial_pass_bp":10000,"trial_ci95_low_bp":10000,"trial_ci95_high_bp":10000,"task_pass_bp":10000,"task_ci95_low_bp":10000,"task_ci95_high_bp":10000' || {
  echo "FAIL: validate-results json pass-at-k confidence totals"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q '"arms":\[{"arm":"habu-forth","rows":1,"certified":1,"first_tests_passed":1,"tests_passed":1,"repair_iterations":0,"checker_iterations":1,"diagnostic_count":0,"tokens_used":0,"wall_ms":0,"final_chars":1,"task_groups":1,"task_pass_at_k":1,"trial_pass_bp":10000,"trial_ci95_low_bp":10000,"trial_ci95_high_bp":10000,"task_pass_bp":10000,"task_ci95_low_bp":10000,"task_ci95_high_bp":10000},{"arm":"habu-forth-raw","rows":1,"certified":1,"first_tests_passed":1,"tests_passed":1,"repair_iterations":0,"checker_iterations":1,"diagnostic_count":0,"tokens_used":0,"wall_ms":0,"final_chars":1,"task_groups":1,"task_pass_at_k":1,"trial_pass_bp":10000,"trial_ci95_low_bp":10000,"trial_ci95_high_bp":10000,"task_pass_bp":10000,"task_ci95_low_bp":10000,"task_ci95_high_bp":10000},{"arm":"habu-forth-blind","rows":1,"certified":1,"first_tests_passed":1,"tests_passed":1,"repair_iterations":0,"checker_iterations":1,"diagnostic_count":0,"tokens_used":0,"wall_ms":0,"final_chars":1,"task_groups":1,"task_pass_at_k":1,"trial_pass_bp":10000,"trial_ci95_low_bp":10000,"trial_ci95_high_bp":10000,"task_pass_bp":10000,"task_ci95_low_bp":10000,"task_ci95_high_bp":10000}\]' || {
  echo "FAIL: validate-results json arms"
  printf '%s\n' "$out"
  exit 1
}

write_multi_model_jsonl
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl)
printf '%s\n' "$out" | grep -q 'run=multi-model-fixture-2026-06-18 model=multiple rows=2 certified=2 first_tests=2 tests=2 repairs=0 checker_iterations=2 diagnostics=0 tokens=0 wall_ms=0' || {
  echo "FAIL: validate-results multi-model text summary"
  printf '%s\n' "$out"
  exit 1
}

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" --json bench/llm/results/live.jsonl)
printf '%s\n' "$out" | grep -q '"run_id":"multi-model-fixture-2026-06-18","model":"multiple","rows":2' || {
  echo "FAIL: validate-results multi-model json summary"
  printf '%s\n' "$out"
  exit 1
}

write_confidence_jsonl
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl)
printf '%s\n' "$out" | grep -q 'pass_at_k task_groups=1 task_passed=1 trial_pass_bp=5000 trial_ci95_low_bp=0 trial_ci95_high_bp=10000 task_pass_bp=10000 task_ci95_low_bp=10000 task_ci95_high_bp=10000' || {
  echo "FAIL: validate-results confidence text summary"
  printf '%s\n' "$out"
  exit 1
}

out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" --json bench/llm/results/live.jsonl)
printf '%s\n' "$out" | grep -q '"rows":2,"task_groups":1,"task_pass_at_k":1,"trial_pass_bp":5000,"trial_ci95_low_bp":0,"trial_ci95_high_bp":10000,"task_pass_bp":10000,"task_ci95_low_bp":10000,"task_ci95_high_bp":10000' || {
  echo "FAIL: validate-results confidence json summary"
  printf '%s\n' "$out"
  exit 1
}

write_live_jsonl 2
tail -n +2 "$T/bench/llm/results/live.jsonl" > "$T/bench/llm/results/live.missing-row"
mv "$T/bench/llm/results/live.missing-row" "$T/bench/llm/results/live.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted missing k trial"; exit 1; }
printf '%s\n' "$out" | grep -q 'k_trials coverage mismatch task=1 model=toy-model arm=forth rows=1 k_trials=2' || {
  echo "FAIL: validate-results missing k-trial diagnostic"
  printf '%s\n' "$out"
  exit 1
}

write_live_jsonl 1
sed -n '1p' "$T/bench/llm/results/live.jsonl" |
  sed 's/:1:1"/:1:2"/; s/"trial":1,/"trial":2,/' >> "$T/bench/llm/results/live.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted extra k trial"; exit 1; }
printf '%s\n' "$out" | grep -q 'k_trials coverage mismatch task=1 model=toy-model arm=forth rows=2 k_trials=1' || {
  echo "FAIL: validate-results extra k-trial diagnostic"
  printf '%s\n' "$out"
  exit 1
}

write_live_jsonl 1
awk 'NR == 1 {
  sub(/,"task_family":"[^"]+"/, "");
  sub(/,"model_version":"[^"]+"/, "");
  sub(/,"model_date":"[^"]+"/, "");
  sub(/,"source_chars":[0-9][0-9]*/, "");
} { print }' "$T/bench/llm/results/live.jsonl" > "$T/bench/llm/results/live.legacy-v2"
mv "$T/bench/llm/results/live.legacy-v2" "$T/bench/llm/results/live.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted legacy-shaped V2 row"; exit 1; }
printf '%s\n' "$out" | grep -q 'missing fields task_family' || {
  echo "FAIL: validate-results legacy V2 missing-field diagnostic"
  printf '%s\n' "$out"
  exit 1
}

write_live_jsonl 1
dup_line=$(sed -n '1p' "$T/bench/llm/results/live.jsonl")
printf '%s\n' "$dup_line" >> "$T/bench/llm/results/live.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted duplicate V2 identity"; exit 1; }
printf '%s\n' "$out" | grep -q 'duplicate result identity' || {
  echo "FAIL: validate-results V2 duplicate diagnostic"
  printf '%s\n' "$out"
  exit 1
}

write_live_jsonl 1
awk 'NR == 1 { sub(/,"raw_response_sha256":"[0-9a-f][0-9a-f]*"/, "") } { print }' \
  "$T/bench/llm/results/live.jsonl" > "$T/bench/llm/results/live.missing-artifact"
mv "$T/bench/llm/results/live.missing-artifact" "$T/bench/llm/results/live.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted missing replay hash"; exit 1; }
printf '%s\n' "$out" | grep -q 'missing fields raw_response_sha256' || {
  echo "FAIL: validate-results missing replay hash diagnostic"
  printf '%s\n' "$out"
  exit 1
}

write_live_jsonl 1
awk 'NR == 1 { sub(/"final_bundle_sha256":"[0-9a-f][0-9a-f]*"/, "\"final_bundle_sha256\":\"not-a-sha\"") } { print }' \
  "$T/bench/llm/results/live.jsonl" > "$T/bench/llm/results/live.bad-hash"
mv "$T/bench/llm/results/live.bad-hash" "$T/bench/llm/results/live.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" bench/llm/results/live.jsonl 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted invalid replay hash"; exit 1; }
printf '%s\n' "$out" | grep -q 'invalid sha256 hash' || {
  echo "FAIL: validate-results invalid replay hash diagnostic"
  printf '%s\n' "$out"
  exit 1
}

write_reference_jsonl
dup_line=$(sed -n '1p' "$T/bench/llm/results/reference.jsonl")
printf '%s\n' "$dup_line" >> "$T/bench/llm/results/reference.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted duplicate"; exit 1; }
printf '%s\n' "$out" | grep -q 'duplicate task_id' || {
  echo "FAIL: validate-results duplicate diagnostic"
  printf '%s\n' "$out"
  exit 1
}

write_reference_jsonl
awk '{
  gsub(/reference-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/, "reference-2026-02-29");
  print
}' "$T/bench/llm/results/reference.jsonl" > "$T/bench/llm/results/reference.bad-date"
mv "$T/bench/llm/results/reference.bad-date" "$T/bench/llm/results/reference.jsonl"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted invalid run_id date"; exit 1; }
printf '%s\n' "$out" | grep -q 'invalid run_id date' || {
  echo "FAIL: validate-results invalid run_id diagnostic"
  printf '%s\n' "$out"
  exit 1
}

write_reference_jsonl
awk 'BEGIN { FS=OFS="\t" } NR > 1 && $4 == "aot-safe" { $4 = "parsing" } { print }' \
  "$ROOT/bench/llm/tasks.tsv" > "$T/bench/llm/tasks.tsv"
set +e
out=$(cd "$T" && "$ROOT/bin/hb" "$BUNDLE" 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: validate-results accepted missing category"; exit 1; }
printf '%s\n' "$out" | grep -q 'missing required benchmark category aot-safe' || {
  echo "FAIL: validate-results missing category diagnostic"
  printf '%s\n' "$out"
  exit 1
}

echo "PASS: validate-results fixtures"
