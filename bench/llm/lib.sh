# lib.sh — shared helpers for the array-algorithm benchmark. POSIX sh; source it.
# Tasks operate on an INTEGER ARRAY. Two calling conventions (conv):
#   as : (array) -> scalar         e.g. ARR-SUM [3 1 4] -> 8
#   aa : (array) -> array          e.g. REVERSE [3 1 4] -> [4 1 3]   (habu: in place)
# Vectors use [..] for arrays. Single source of truth = bench/llm/tasks.tsv; each arm's
# test harness is GENERATED from the same vectors.
#   habu: builds the array in memory (here , ,) and calls ( ptr n -- ... );
#   JS:   f(arr)  returns a number (as) or array (aa);
#   Python:     f(list) returns an int (as) or a new list (aa);
#   TypeScript: f(array) returns a number (as) or array (aa);
#   Rust: fn f(a:&[i64]) -> i64 (as) or -> Vec<i64> (aa).

BENCH_TASK_HEADER=$(printf 'id\tname\tsignature\tcategory\ttests\tharness\tconv\tspec\tvectors\ttags\tjs_signature\trust_signature')

bench_require_manifest_header() {
  _manifest=$1
  _got=$(sed -n '1p' "$_manifest")
  [ "$_got" = "$BENCH_TASK_HEADER" ] && return 0
  echo "bench/llm: unsupported task manifest schema in $_manifest" >&2
  echo "bench/llm: expected: $BENCH_TASK_HEADER" >&2
  echo "bench/llm: got:      $_got" >&2
  return 64
}

bench_sig() {
  printf '%s' "$1" | sed 's/^[(]//; s/[)]$//' | xargs
}

now_ms() { perl -MTime::HiRes=time -e 'printf "%d", time()*1000'; }
unbr()   { printf '%s' "$1" | tr -d '[]' | xargs; }              # "[3 1 4]" -> "3 1 4"
_lhs()   { printf '%s' "$1" | sed 's/->.*//'; }
_rhs()   { printf '%s' "$1" | sed 's/.*->//'; }

MODEL_REGISTRY=${MODEL_REGISTRY:-bench/llm/models.tsv}

model_ids() {
  awk -F '\t' 'NR > 1 && $1 != "" && substr($1, 1, 1) != "#" { print $1 }' "$MODEL_REGISTRY"
}

model_row() {
  awk -F '\t' -v id="$1" 'NR > 1 && $1 == id { print; found = 1; exit }
    END { if (!found) exit 1 }' "$MODEL_REGISTRY"
}

model_init() {
  _mid=${MODEL_ID:-${MODEL:-claude}}
  _row=$(model_row "$_mid") || {
    echo "bench/llm: unknown model id $_mid in $MODEL_REGISTRY" >&2
    exit 64
  }
  _model_field() {
    printf '%s\n' "$_row" | awk -F '\t' -v n="$1" '{ print $n }'
  }
  MODEL_ID=$(_model_field 1)
  MODEL_LABEL=$(_model_field 2)
  MODEL_COMMAND=$(_model_field 3)
  MODEL_ARGS=$(_model_field 4)
  MODEL_PARSER=$(_model_field 5)
  MODEL_TOKEN_FIELDS=$(_model_field 6)
  MODEL_TIMEOUT=$(_model_field 7)
  [ -n "$MODEL_LABEL" ] || MODEL_LABEL=$MODEL_ID
  [ -n "$MODEL_COMMAND" ] || MODEL_COMMAND=$MODEL_ID
  [ -n "$MODEL_ARGS" ] || MODEL_ARGS='{prompt}'
  [ -n "$MODEL_PARSER" ] || MODEL_PARSER=raw
  [ -n "$MODEL_TIMEOUT" ] || MODEL_TIMEOUT=120
  [ -z "${CLAUDE:-}" ] || MODEL_COMMAND=$CLAUDE
  MODEL=${MODEL:-$MODEL_LABEL}
}

model_run() {
  _prompt=$1
  _out=$2
  case "$MODEL_ARGS" in
    '-p {prompt} --output-format json')
      timeout "$MODEL_TIMEOUT" "$MODEL_COMMAND" -p "$_prompt" --output-format json > "$_out" 2>/dev/null
      ;;
    '{prompt}')
      timeout "$MODEL_TIMEOUT" "$MODEL_COMMAND" "$_prompt" > "$_out" 2>/dev/null
      ;;
    '')
      timeout "$MODEL_TIMEOUT" "$MODEL_COMMAND" "$_prompt" > "$_out" 2>/dev/null
      ;;
    *)
      echo "bench/llm: unsupported args template for $MODEL_ID: $MODEL_ARGS" >&2
      return 64
      ;;
  esac
}

# hb_test <conv> <NAME> <vectors> -> habu assertions (AP holds the array pointer).
hb_test() {
  _conv=$1; _name=$2
  printf '%s\n' "$3" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    IFS=' '   # force space splitting below regardless of the caller's IFS
    av=$(unbr "$(_lhs "$p")"); n=$(printf '%s' "$av" | wc -w | tr -d ' ')
    build="here"; for v in $av; do build="$build $v ,"; done; build="$build AP !"
    if [ "$_conv" = as ]; then
      e=$(printf '%s' "$(_rhs "$p")" | xargs)
      printf '%s  AP @ %s %s %s G=\n' "$build" "$n" "$_name" "$e"
    else
      ev=$(unbr "$(_rhs "$p")"); line="$build  AP @ $n $_name "
      j=0; for e in $ev; do line="$line AP @ $j cells + @ $e G= "; j=$((j+1)); done
      printf '%s\n' "$line"
    fi
  done
}

# js_test <conv> <vectors> -> "check(f([..]), <num|array>, "..");"  (f returns num/array)
js_test() {
  _conv=$1
  printf '%s\n' "$2" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    a=$(unbr "$(_lhs "$p")" | tr ' ' ',')
    if [ "$_conv" = as ]; then e=$(printf '%s' "$(_rhs "$p")" | xargs)
    else e="[$(unbr "$(_rhs "$p")" | tr ' ' ',')]"; fi
    printf '  check(f([%s]), %s, "[%s]");\n' "$a" "$e" "$a"
  done
}

# rust_test <conv> <vectors> -> "assert_eq!(f(&[..]), <e | vec![..]>);"
rust_test() {
  _conv=$1
  printf '%s\n' "$2" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    a=$(unbr "$(_lhs "$p")" | tr ' ' ',')
    if [ "$_conv" = as ]; then e=$(printf '%s' "$(_rhs "$p")" | xargs)
    else e="vec![$(unbr "$(_rhs "$p")" | tr ' ' ',')]"; fi
    printf '    assert_eq!(f(&[%s]), %s);\n' "$a" "$e"
  done
}

py_test() {
  _conv=$1
  printf '%s\n' "$2" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    a=$(unbr "$(_lhs "$p")" | tr ' ' ',')
    if [ "$_conv" = as ]; then e=$(printf '%s' "$(_rhs "$p")" | xargs)
    else e="[$(unbr "$(_rhs "$p")" | tr ' ' ',')]"; fi
    printf 'check(f([%s]), %s, "[%s]")\n' "$a" "$e" "$a"
  done
}

ts_test() {
  _conv=$1
  printf '%s\n' "$2" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    a=$(unbr "$(_lhs "$p")" | tr ' ' ',')
    if [ "$_conv" = as ]; then e=$(printf '%s' "$(_rhs "$p")" | xargs)
    else e="[$(unbr "$(_rhs "$p")" | tr ' ' ',')]"; fi
    printf 'check(f([%s]), %s, "[%s]");\n' "$a" "$e" "$a"
  done
}

hb_bench() {
  _conv=$1; _name=$2
  printf '%s\n' "$3" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    IFS=' '
    av=$(unbr "$(_lhs "$p")"); n=$(printf '%s' "$av" | wc -w | tr -d ' ')
    build="here"; for v in $av; do build="$build $v ,"; done; build="$build AP !"
    if [ "$_conv" = as ]; then
      printf '%s  AP @ %s %s drop\n' "$build" "$n" "$_name"
    else
      printf '%s  AP @ %s %s\n' "$build" "$n" "$_name"
    fi
  done
}

js_bench() {
  printf 'function benchOnce(){\n'
  printf '%s\n' "$1" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    a=$(unbr "$(_lhs "$p")" | tr ' ' ',')
    printf '  void f([%s]);\n' "$a"
  done
  printf '}\n'
  printf 'for (let i = 0; i < %s; i++) benchOnce();\n' "$(bench_runtime_warmups)"
  printf 'const __benchStart = process.hrtime.bigint();\n'
  printf 'for (let i = 0; i < %s; i++) benchOnce();\n' "$(bench_runtime_repetitions)"
  printf 'const __benchNs = process.hrtime.bigint() - __benchStart;\n'
  printf 'const __benchMs = Number((__benchNs + 999999n) / 1000000n);\n'
  printf 'console.log("RUNTIME-MS " + __benchMs);\n'
}

py_bench() {
  printf 'import time\n'
  printf 'def bench_once():\n'
  printf '%s\n' "$1" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    a=$(unbr "$(_lhs "$p")" | tr ' ' ',')
    printf '    f([%s])\n' "$a"
  done
  printf 'for _ in range(%s):\n' "$(bench_runtime_warmups)"
  printf '    bench_once()\n'
  printf '__bench_start = time.perf_counter_ns()\n'
  printf 'for _ in range(%s):\n' "$(bench_runtime_repetitions)"
  printf '    bench_once()\n'
  printf '__bench_ns = time.perf_counter_ns() - __bench_start\n'
  printf '__bench_ms = (__bench_ns + 999_999) // 1_000_000\n'
  printf 'print("RUNTIME-MS " + str(__bench_ms))\n'
}

ts_bench() {
  printf 'function benchOnce(): void {\n'
  printf '%s\n' "$1" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    a=$(unbr "$(_lhs "$p")" | tr ' ' ',')
    printf '  void f([%s]);\n' "$a"
  done
  printf '}\n'
  printf 'for (let i = 0; i < %s; i++) benchOnce();\n' "$(bench_runtime_warmups)"
  printf 'const __benchStart = process.hrtime.bigint();\n'
  printf 'for (let i = 0; i < %s; i++) benchOnce();\n' "$(bench_runtime_repetitions)"
  printf 'const __benchNs = process.hrtime.bigint() - __benchStart;\n'
  printf 'const __benchMs = Number((__benchNs + 999999n) / 1000000n);\n'
  printf 'console.log("RUNTIME-MS " + __benchMs);\n'
}

rust_bench() {
  printf '    fn bench_once() {\n'
  printf '%s\n' "$1" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    a=$(unbr "$(_lhs "$p")" | tr ' ' ',')
    printf '        std::hint::black_box(f(&[%s]));\n' "$a"
  done
  printf '    }\n'
  printf '    for _ in 0..%s { bench_once(); }\n' "$(bench_runtime_warmups)"
  printf '    let __bench_start = std::time::Instant::now();\n'
  printf '    for _ in 0..%s { bench_once(); }\n' "$(bench_runtime_repetitions)"
  printf '    let __bench_ms = (__bench_start.elapsed().as_nanos() + 999_999) / 1_000_000;\n'
  printf '    println!("RUNTIME-MS {}", __bench_ms);\n'
}

js_ret()   { [ "$1" = as ] && printf 'a single integer' || printf 'a NEW array of integers'; }
py_ret()   { [ "$1" = as ] && printf 'a single integer' || printf 'a NEW list of integers'; }
ts_ret()   { [ "$1" = as ] && printf 'a single number' || printf 'a NEW array of numbers'; }
rust_ret() { [ "$1" = as ] && printf 'i64' || printf 'Vec<i64>'; }
ts_ret_type() { [ "$1" = as ] && printf 'number' || printf 'number[]'; }

case_list() {
  printf '%s\n' "$1" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    printf '  %s\n' "$(printf '%s' "$p" | xargs)"
  done
}

bench_runtime_repetitions() {
  printf '%s' "${BENCH_RUNTIME_REPETITIONS:-${BENCH_RUNTIME_REPS:-100}}"
}

bench_runtime_warmups() {
  printf '%s' "${BENCH_RUNTIME_WARMUPS:-10}"
}

bench_trial_id() {
  printf '%s:%s:%s:%s:%s' \
    "${BENCH_SEED:-manifest}" "${MODEL_ID:-$3}" "$2" "$1" "${BENCH_TRIAL:-0}"
}

bench_runtime_ms_from_output() {
  awk '
    /^[0-9][0-9]*$/ { ms = $1 }
    $1 == "RUNTIME-MS" && $2 ~ /^[0-9][0-9]*$/ { ms = $2 }
    END { if (ms != "") print ms; else exit 1 }
  '
}

bench_run_id() {
  printf '%s' "${BENCH_RUN_ID:-${BENCH_SEED:-manifest}-$(date +%F)}"
}

bench_task_family() {
  printf '%s' "${BENCH_TASK_FAMILY:-arrays}"
}

bench_model_version() {
  printf '%s' "${BENCH_MODEL_VERSION:-${MODEL_VERSION:-unknown}}"
}

bench_model_date() {
  printf '%s' "${BENCH_MODEL_DATE:-${MODEL_DATE:-unknown}}"
}

bench_json_quote() {
  node -e 'process.stdout.write(JSON.stringify(process.argv[1] || ""))' "$1"
}

bench_json_quote_file() {
  _path=$1
  if [ -n "$_path" ] && [ -f "$_path" ]; then
    node -e 'const fs=require("fs"); process.stdout.write(JSON.stringify(fs.readFileSync(process.argv[1], "utf8")))' "$_path"
  else
    bench_json_quote ""
  fi
}

bench_sha256_file() {
  _path=$1
  if [ -n "$_path" ] && [ -f "$_path" ]; then
    if command -v shasum >/dev/null 2>&1; then shasum -a 256 "$_path"; else sha256sum "$_path"; fi
  else
    if command -v shasum >/dev/null 2>&1; then printf '' | shasum -a 256; else printf '' | sha256sum; fi
  fi | awk '{ print $1 }'
}

bench_file_chars() {
  _path=$1
  if [ -n "$_path" ] && [ -f "$_path" ]; then
    _n=$(wc -c < "$_path" | tr -d ' ')
  else
    _n=0
  fi
  [ "$_n" -gt 0 ] || _n=1
  printf '%s' "$_n"
}

bench_artifact_json() {
  _field=$1
  _path=$2
  _body=$(bench_json_quote_file "$_path")
  _hash=$(bench_sha256_file "$_path")
  printf '"%s":%s,"%s_sha256":"%s"' "$_field" "$_body" "$_field" "$_hash"
}

# emit_row: id name model arm outcome rounds tokens wall_ms [runtime_ms] [runtime_status]
emit_row() {
  fp=false; [ "$5" = pass ] && [ "$6" -eq 1 ] && fp=true
  tests_passed=false; [ "$5" = pass ] && tests_passed=true
  checker_verdict=rejected; [ "$5" = pass ] && checker_verdict=certified
  repair_iterations=0; [ "$6" -gt 0 ] && repair_iterations=$(( $6 - 1 ))
  runtime_ms=${9:-null}
  runtime_status=${10:-not_run}
  run_id=$(bench_run_id)
  trial_id=$(bench_trial_id "$1" "$4" "$3")
  source_file=${BENCH_SOURCE_FILE:-${BENCH_CANDIDATE_FILE:-}}
  source_chars=${BENCH_SOURCE_CHARS:-$(bench_file_chars "$source_file")}
  name_q=$(bench_json_quote "$2")
  model_id_q=$(bench_json_quote "${MODEL_ID:-$3}")
  model_q=$(bench_json_quote "$3")
  arm_q=$(bench_json_quote "$4")
  run_id_q=$(bench_json_quote "$run_id")
  trial_id_q=$(bench_json_quote "$trial_id")
  family_q=$(bench_json_quote "$(bench_task_family)")
  model_version_q=$(bench_json_quote "$(bench_model_version)")
  model_date_q=$(bench_json_quote "$(bench_model_date)")
  seed_q=$(bench_json_quote "${BENCH_SEED:-manifest}")
  outcome_q=$(bench_json_quote "$5")
  runtime_status_q=$(bench_json_quote "$runtime_status")
  printf '{'
  printf '"schema_version":2,"run_id":%s,' "$run_id_q"
  printf '"task_id":%s,"name":%s,"model_id":%s,"model":%s,"arm":%s,"trial_id":%s,' \
    "$1" "$name_q" "$model_id_q" "$model_q" "$arm_q" "$trial_id_q"
  printf '"trial":%s,"task_order":%s,"k_trials":%s,"order_seed":%s,' \
    "${BENCH_TRIAL:-0}" "${BENCH_TASK_ORDER:-0}" "${BENCH_K:-0}" "$seed_q"
  printf '"outcome":%s,"rounds":%s,"first_pass":%s,"tokens":%s,"wall_ms":%s,"source_chars":%s,' \
    "$outcome_q" "$6" "$fp" "$7" "$8" "$source_chars"
  printf '"runtime_ms":%s,"runtime_repetitions":%s,"runtime_warmups":%s,"runtime_status":%s,' \
    "$runtime_ms" "$(bench_runtime_repetitions)" "$(bench_runtime_warmups)" "$runtime_status_q"
  printf '"task_family":%s,"model_version":%s,"model_date":%s,' "$family_q" "$model_version_q" "$model_date_q"
  printf '"attempt":%s,"first_pass_checker":"%s","first_pass_tests":%s,"tests_passed":%s,' \
    "$6" "$checker_verdict" "$fp" "$tests_passed"
  printf '"repair_iterations":%s,"checker_iterations":%s,"diagnostic_count":%s,' \
    "$repair_iterations" "$6" "${BENCH_DIAGNOSTIC_COUNT:-0}"
  printf '"diagnostic_token":%s,"diagnostic_span":%s,"diagnostic_expected":%s,"diagnostic_actual":%s,' \
    "${BENCH_DIAGNOSTIC_TOKEN:-true}" "${BENCH_DIAGNOSTIC_SPAN:-true}" \
    "${BENCH_DIAGNOSTIC_EXPECTED:-true}" "${BENCH_DIAGNOSTIC_ACTUAL:-true}"
  printf '"diagnostic_code":%s,"diagnostic_repair_class":%s,"all_errors_stable":%s,' \
    "${BENCH_DIAGNOSTIC_CODE:-true}" "${BENCH_DIAGNOSTIC_REPAIR_CLASS:-true}" "${BENCH_ALL_ERRORS_STABLE:-true}"
  printf '"tokens_used":%s,"final_chars":%s,"trust_uses":%s,"signature_weakened":%s,' \
    "$7" "$source_chars" "${BENCH_TRUST_USES:-0}" "${BENCH_SIGNATURE_WEAKENED:-false}"
  bench_artifact_json prompt "${BENCH_PROMPT_FILE:-}"; printf ','
  bench_artifact_json raw_response "${BENCH_RAW_RESPONSE_FILE:-}"; printf ','
  bench_artifact_json extracted_candidate "${BENCH_CANDIDATE_FILE:-}"; printf ','
  bench_artifact_json checker_diagnostics "${BENCH_CHECKER_DIAGNOSTICS_FILE:-}"; printf ','
  bench_artifact_json repair_packet "${BENCH_REPAIR_PACKET_FILE:-}"; printf ','
  bench_artifact_json test_output "${BENCH_TEST_OUTPUT_FILE:-}"; printf ','
  bench_artifact_json final_bundle "${BENCH_FINAL_BUNDLE_FILE:-}"
  printf '}\n'
}
