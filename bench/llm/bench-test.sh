#!/bin/sh
# bench-test.sh — deterministic teeth for the array-algorithm drivers using STUB
# models (no real claude, no tokens). Covers both conventions (array->scalar,
# array->array) across raw Habu, library-assisted Habu, JS, Python,
# TypeScript, and Rust, plus repair loops.
set -e
cd "$(dirname "$0")/../.."
T=$(mktemp -d "${TMPDIR:-/tmp}/bench-test.XXXXXX"); trap 'rm -rf "$T"' EXIT
fails=0
mkstub() { printf '#!/bin/sh\n%s\n' "$2" > "$1"; chmod +x "$1"; }
chk() { if printf '%s' "$3" | grep -q "$2"; then echo "ok: $1"; else echo "FAIL: $1 -> $3"; fails=$((fails+1)); fi; }

manifest_ready=1
expected_manifest_header=$(printf 'id\tname\tsignature\tcategory\ttests\tharness\tconv\tspec\tvectors\ttags\tjs_signature\trust_signature')
manifest_header=$(sed -n '1p' bench/llm/tasks.tsv)
if [ "$manifest_header" = "$expected_manifest_header" ]; then
  echo "ok: manifest-v2-header"
else
  echo "FAIL: manifest-v2-header -> $manifest_header"
  fails=$((fails+1))
  manifest_ready=0
fi

if grep -q 'bench-tasks.tsv' bench/llm/run-bench.sh; then
  echo "FAIL: run-bench-canonical-default -> still mentions bench-tasks.tsv"
  fails=$((fails+1))
  manifest_ready=0
else
  echo "ok: run-bench-canonical-default"
fi

if awk 'NF && substr($0, 1, 1) != "#" { found = 1 } END { exit found ? 1 : 0 }' bench/llm/bench-tasks.tsv; then
  echo "ok: retired-bench-tasks"
else
  echo "FAIL: retired-bench-tasks -> bench-tasks.tsv still has task rows"
  fails=$((fails+1))
  manifest_ready=0
fi

printf '{"result":": X ;","modelUsage":{"claude-opus-4":{"outputTokens":7}}}\n' > "$T/resp.json"
rt=$(sh bench/llm/parse-resp.sh "$T/resp.json" "$T/resp.txt")
[ "$rt" = 7 ] && [ "$(cat "$T/resp.txt")" = ': X ;' ] && echo "ok: parse-resp-modelUsage" || {
  echo "FAIL: parse-resp-modelUsage -> tokens=$rt text=$(cat "$T/resp.txt")"
  fails=$((fails+1))
}

printf '{"choices":[{"message":{"content":"function f(a){return a.length;}"}}],"usage":{"completion_tokens":11}}\n' > "$T/openai.json"
rt=$(sh bench/llm/parse-resp.sh "$T/openai.json" "$T/openai.txt" openai-json usage.completion_tokens)
[ "$rt" = 11 ] && [ "$(cat "$T/openai.txt")" = 'function f(a){return a.length;}' ] && echo "ok: parse-resp-openai" || {
  echo "FAIL: parse-resp-openai -> tokens=$rt text=$(cat "$T/openai.txt")"
  fails=$((fails+1))
}

cat > "$T/codex.jsonl" <<'EOF'
{"type":"thread.started","thread_id":"fixture"}
{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"function f(a){return a.length;}"}}
{"type":"turn.completed","usage":{"input_tokens":100,"output_tokens":13,"reasoning_output_tokens":5}}
EOF
rt=$(sh bench/llm/parse-resp.sh "$T/codex.jsonl" "$T/codex.txt" codex-jsonl usage.output_tokens)
[ "$rt" = 13 ] && [ "$(cat "$T/codex.txt")" = 'function f(a){return a.length;}' ] && echo "ok: parse-resp-codex" || {
  echo "FAIL: parse-resp-codex -> tokens=$rt text=$(cat "$T/codex.txt")"
  fails=$((fails+1))
}

cat > "$T/report.jsonl" <<'EOF'
{"task_id":1,"name":"ZERO-TOK","model":"fixture","arm":"habu-a","outcome":"pass","rounds":1,"first_pass":true,"tokens":0,"wall_ms":10}
{"task_id":1,"name":"ZERO-TOK","model":"fixture","arm":"js","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":10}
EOF
rep=$(sh bench/llm/report.sh "$T/report.jsonl")
chk report-zero-token-note 'exclude 1 passing row' "$rep"
chk report-zero-token-table '| ZERO-TOK | — | — | — | — | 5 | — | — | — | — | — | — | — |' "$rep"

cat > "$T/report-models.jsonl" <<'EOF'
{"task_id":1,"name":"MREG","model":"alpha","arm":"js","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":10}
{"task_id":1,"name":"MREG","model":"beta","arm":"js","outcome":"fail","rounds":2,"first_pass":false,"tokens":9,"wall_ms":20}
EOF
rep=$(sh bench/llm/report.sh "$T/report-models.jsonl")
chk report-model-section '## Per-Model Reliability' "$rep"
chk report-model-alpha '| alpha | JavaScript | 1 | 1 | 100% | 100% | 100% | 0 |' "$rep"
chk report-model-beta '| beta | JavaScript | 1 | 0 | 0% | 0% | 0% | 1 |' "$rep"

cat > "$T/report-model-family.jsonl" <<'EOF'
{"task_id":1,"name":"MREG","model_id":"alpha","model":"AlphaJS","arm":"js","trial_id":"fixture-seed:alpha:js:1:1","trial":1,"task_order":7,"k_trials":1,"order_seed":"fixture-seed","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":10,"runtime_ms":3,"runtime_repetitions":2,"runtime_warmups":1,"runtime_status":"ok"}
{"task_id":1,"name":"MREG","model_id":"beta","model":"BetaJS","arm":"js","trial_id":"fixture-seed:beta:js:1:1","trial":1,"task_order":7,"k_trials":1,"order_seed":"fixture-seed","outcome":"fail","rounds":2,"first_pass":false,"tokens":9,"wall_ms":20,"runtime_ms":null,"runtime_repetitions":2,"runtime_warmups":1,"runtime_status":"not_run"}
EOF
rep=$(sh bench/llm/report.sh "$T/report-model-family.jsonl")
chk report-family-task-pass-at-k '| JavaScript | 2 | 1 | 50% | 50% | 50% | 1 |' "$rep"

cat > "$T/report-habu-arms.jsonl" <<'EOF'
{"task_id":1,"name":"HARM","model_id":"fixture","model":"Fixture","arm":"habu-stdlib","trial_id":"manifest:fixture:habu-stdlib:1:1","trial":1,"task_order":1,"k_trials":1,"order_seed":"manifest","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":10,"runtime_ms":1,"runtime_repetitions":2,"runtime_warmups":1,"runtime_status":"ok"}
{"task_id":1,"name":"HARM","model_id":"fixture","model":"Fixture","arm":"habu-skeleton","trial_id":"manifest:fixture:habu-skeleton:1:1","trial":1,"task_order":1,"k_trials":1,"order_seed":"manifest","outcome":"pass","rounds":1,"first_pass":true,"tokens":6,"wall_ms":10,"runtime_ms":1,"runtime_repetitions":2,"runtime_warmups":1,"runtime_status":"ok"}
EOF
rep=$(sh bench/llm/report.sh "$T/report-habu-arms.jsonl")
chk report-habu-stdlib-label 'Habu + stdlib' "$rep"
chk report-habu-skeleton-label 'Habu + skeleton' "$rep"
chk report-habu-arms-task-row 'stdlib pass/1; skeleton pass/1' "$rep"

cat > "$T/report-py-ts-arms.jsonl" <<'EOF'
{"task_id":1,"name":"PYTS","model_id":"fixture","model":"Fixture","arm":"python","trial_id":"manifest:fixture:python:1:1","trial":1,"task_order":1,"k_trials":1,"order_seed":"manifest","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":10,"runtime_ms":1,"runtime_repetitions":2,"runtime_warmups":1,"runtime_status":"ok"}
{"task_id":1,"name":"PYTS","model_id":"fixture","model":"Fixture","arm":"ts","trial_id":"manifest:fixture:ts:1:1","trial":1,"task_order":1,"k_trials":1,"order_seed":"manifest","outcome":"pass","rounds":1,"first_pass":true,"tokens":6,"wall_ms":10,"runtime_ms":1,"runtime_repetitions":2,"runtime_warmups":1,"runtime_status":"ok"}
EOF
rep=$(sh bench/llm/report.sh "$T/report-py-ts-arms.jsonl")
chk report-python-label 'Python' "$rep"
chk report-typescript-label 'TypeScript' "$rep"
chk report-py-ts-arms-task-row 'python pass/1; ts pass/1' "$rep"

cat > "$T/report-runtime.jsonl" <<'EOF'
{"task_id":1,"name":"RT","model_id":"fixture","model":"Fixture","arm":"js","trial_id":"manifest:fixture:js:1:1","trial":1,"task_order":1,"k_trials":1,"order_seed":"manifest","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":9000,"runtime_ms":7,"runtime_repetitions":2,"runtime_warmups":1,"runtime_status":"ok"}
EOF
rep=$(sh bench/llm/report.sh "$T/report-runtime.jsonl")
chk report-runtime-header 'median runtime ms' "$rep"
chk report-runtime-not-wall '| JavaScript | 1 | 5 | \*\*5\*\* | 5 | 7 | 7 | 9 | 9 |' "$rep"
chk report-limitations-section '## Limitations' "$rep"
chk report-limitations-nondeterminism 'nondeterminism' "$rep"
chk report-limitations-confidence 'k/N confidence' "$rep"
chk report-limitations-token-proxy 'token proxy limits' "$rep"
chk report-limitations-scaffold 'scaffold fairness' "$rep"
chk report-limitations-library 'library comparability' "$rep"
chk report-limitations-task-selection 'task selection' "$rep"
chk report-limitations-environment 'environment' "$rep"
chk report-limitations-boundary 'deterministic-vs-live boundary' "$rep"
chk report-evidence-contract 'run_id.*model_id.*arm.*task_id.*trial_id' "$rep"
chk report-replay-artifacts 'prompt.*raw_response.*extracted_candidate.*checker_diagnostics.*repair_packet.*test_output.*final_bundle' "$rep"

long_name=LONG-DYNAMIC-STORAGE-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
: > "$T/report-large.jsonl"
i=1
while [ "$i" -le 1100 ]; do
  printf '{"task_id":1,"name":"%s","model_id":"fixture","model":"Fixture Model With Long Repeated Label","arm":"js","trial_id":"large:%s","trial":%s,"task_order":1,"k_trials":1100,"order_seed":"fixture","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":10,"runtime_ms":1,"runtime_repetitions":2,"runtime_warmups":1,"runtime_status":"ok"}\n' "$long_name" "$i" "$i" >> "$T/report-large.jsonl"
  i=$((i+1))
done
rep=$(sh bench/llm/report.sh "$T/report-large.jsonl")
chk report-dynamic-row-cap 'Generated from `results/run.jsonl` (1100 trials)' "$rep"
chk report-dynamic-string-cap "$long_name" "$rep"

cat > "$T/perf.json" <<'EOF'
{"schema_version":1,"bench":"llm-perf","full":false,"results":[{"name":"check_solutions","wall_ms":12},{"name":"functional_tests","wall_ms":23},{"name":"metric_validator","wall_ms":34},{"name":"prop_smoke_250","wall_ms":45},{"name":"microbench_smoke","wall_ms":56}]}
EOF
rep=$(sh bench/llm/report.sh "$T/report-runtime.jsonl" "$T/perf.json")
chk report-latency-section '## LLM Feedback Latency' "$rep"
chk report-latency-source 'bench/llm/perf.sh --json' "$rep"
chk report-latency-validator '| metric_validator | 34 | 0.03 |' "$rep"
chk report-latency-microbench '| microbench_smoke | 56 | 0.06 |' "$rep"

cat > "$T/report-category-deltas.jsonl" <<'EOF'
{"task_id":1,"name":"ARR-A","model_id":"fixture","model":"Fixture","arm":"habu-a","task_family":"arrays","outcome":"pass","rounds":1,"first_pass":true,"tokens":100,"wall_ms":10,"runtime_ms":10,"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,"all_errors_stable":true}
{"task_id":2,"name":"ARR-B","model_id":"fixture","model":"Fixture","arm":"habu-a","task_family":"arrays","outcome":"fail","rounds":2,"first_pass":false,"tokens":0,"wall_ms":20,"runtime_ms":null,"diagnostic_token":false,"diagnostic_span":false,"diagnostic_expected":false,"diagnostic_actual":false,"diagnostic_code":false,"diagnostic_repair_class":false,"all_errors_stable":false}
{"task_id":1,"name":"ARR-A","model_id":"fixture","model":"Fixture","arm":"habu-stdlib","task_family":"arrays","outcome":"pass","rounds":1,"first_pass":true,"tokens":50,"wall_ms":10,"runtime_ms":8,"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,"all_errors_stable":true}
{"task_id":2,"name":"ARR-B","model_id":"fixture","model":"Fixture","arm":"habu-stdlib","task_family":"arrays","outcome":"pass","rounds":1,"first_pass":true,"tokens":60,"wall_ms":10,"runtime_ms":12,"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,"all_errors_stable":true}
{"task_id":1,"name":"ARR-A","model_id":"fixture","model":"Fixture","arm":"habu-skeleton","task_family":"arrays","outcome":"pass","rounds":1,"first_pass":true,"tokens":80,"wall_ms":10,"runtime_ms":20,"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,"all_errors_stable":true}
{"task_id":2,"name":"ARR-B","model_id":"fixture","model":"Fixture","arm":"habu-skeleton","task_family":"arrays","outcome":"fail","rounds":2,"first_pass":false,"tokens":0,"wall_ms":20,"runtime_ms":null,"diagnostic_token":false,"diagnostic_span":false,"diagnostic_expected":false,"diagnostic_actual":false,"diagnostic_code":false,"diagnostic_repair_class":false,"all_errors_stable":false}
{"task_id":3,"name":"STR-A","model_id":"fixture","model":"Fixture","arm":"habu-a","task_family":"strings","outcome":"pass","rounds":1,"first_pass":true,"tokens":200,"wall_ms":10,"runtime_ms":30,"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,"all_errors_stable":true}
{"task_id":3,"name":"STR-A","model_id":"fixture","model":"Fixture","arm":"habu-stdlib","task_family":"strings","outcome":"pass","rounds":1,"first_pass":true,"tokens":100,"wall_ms":10,"runtime_ms":12,"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,"all_errors_stable":true}
{"task_id":3,"name":"STR-A","model_id":"fixture","model":"Fixture","arm":"habu-skeleton","task_family":"strings","outcome":"pass","rounds":1,"first_pass":true,"tokens":90,"wall_ms":10,"runtime_ms":14,"diagnostic_token":true,"diagnostic_span":true,"diagnostic_expected":true,"diagnostic_actual":true,"diagnostic_code":true,"diagnostic_repair_class":true,"all_errors_stable":true}
EOF
rep=$(sh bench/llm/report.sh "$T/report-category-deltas.jsonl")
chk report-category-section '## Category Reliability And Effort' "$rep"
chk report-category-raw-arrays '| arrays | Habu raw | 2 | 1 | 50% | 50% | 1 | 100 | 10 | 50% |' "$rep"
chk report-category-stdlib-arrays '| arrays | Habu + stdlib | 2 | 2 | 100% | 100% | 1 | 55 | 10 | 100% |' "$rep"
chk report-delta-section '## Habu Arm Deltas By Category' "$rep"
chk report-delta-arrays '| arrays | 50% | 100% | 50% | +50pp | -50pp | 0.6x | 1.5x | 1x | 2x |' "$rep"

# --- conv=as : ARR-SUM (array -> scalar) ---
mkstub "$T/hb.sh" 'echo ": ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop ;"'
mkstub "$T/hbl.sh" 'echo ": ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do arr len i A@ + loop ;"'
mkstub "$T/hbstd.sh" 'echo ": ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do arr len i A@ + loop ;"'
mkstub "$T/hbsk.sh" 'echo "0 len 0 ?do i cells arr + @ + loop"'
mkstub "$T/js.sh" 'echo "function f(a){ return a.reduce((s,x)=>s+x,0); }"'
mkstub "$T/py.sh" 'echo "def f(a): return sum(a)"'
mkstub "$T/ts.sh" 'echo "function f(a: number[]): number { return a.reduce((s, x) => s + x, 0); }"'
mkstub "$T/rs.sh" 'echo "fn f(a: &[i64]) -> i64 { a.iter().sum() }"'
SV="[3 1 4] -> 8; [5] -> 5"
r=$(CLAUDE="$T/hb.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" a); chk habu-as '"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbl.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" lib); chk habu-lib-as '"arm":"habu-lib","trial_id":"manifest:claude:habu-lib:1:0","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbstd.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" stdlib); chk habu-stdlib-as '"arm":"habu-stdlib","trial_id":"manifest:claude:habu-stdlib:1:0","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbsk.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" skeleton); chk habu-skeleton-as '"arm":"habu-skeleton","trial_id":"manifest:claude:habu-skeleton:1:0","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/js.sh" sh bench/llm/drive-js.sh   1 ARR-SUM "ptr a n -- i64" "sum" as "$SV");   chk js-as   '"outcome":"pass","rounds":1' "$r"
chk js-as-runtime '"runtime_ms":[0-9][0-9]*,"runtime_repetitions":100,"runtime_warmups":10,"runtime_status":"ok"' "$r"
chk js-v2-schema '"schema_version":2' "$r"
chk js-v2-task-family '"task_family":"arrays"' "$r"
chk js-v2-model-unknown '"model_version":"unknown","model_date":"unknown"' "$r"
chk js-v2-replay-hashes '"prompt_sha256":"[0-9a-f][0-9a-f]*".*"raw_response_sha256":"[0-9a-f][0-9a-f]*".*"final_bundle_sha256":"[0-9a-f][0-9a-f]*"' "$r"
chk js-v2-source-chars '"source_chars":[1-9][0-9]*' "$r"
r=$(CLAUDE="$T/py.sh" sh bench/llm/drive-python.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV"); chk python-as '"arm":"python".*"outcome":"pass","rounds":1' "$r"
chk python-as-runtime '"runtime_ms":[0-9][0-9]*,"runtime_repetitions":100,"runtime_warmups":10,"runtime_status":"ok"' "$r"
r=$(CLAUDE="$T/ts.sh" sh bench/llm/drive-ts.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV"); chk ts-as '"arm":"ts".*"outcome":"pass","rounds":1' "$r"
chk ts-as-runtime '"runtime_ms":[0-9][0-9]*,"runtime_repetitions":100,"runtime_warmups":10,"runtime_status":"ok"' "$r"
r=$(CLAUDE="$T/rs.sh" sh bench/llm/drive-rust.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV");   chk rust-as '"outcome":"pass","rounds":1' "$r"

cat > "$T/models.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
fixture	FixtureJS	$T/js.sh	-p {prompt} --output-format json	raw		5
EOF
r=$(MODEL_REGISTRY="$T/models.tsv" MODEL_ID=fixture sh bench/llm/drive-js.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV")
chk model-registry-label '"model_id":"fixture","model":"FixtureJS","arm":"js","trial_id":"manifest:fixture:js:1:0","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"

cat > "$T/codex-model.sh" <<'EOF'
#!/bin/sh
case "$*" in
  *exec*--json*) ;;
  *) echo "missing codex exec args: $*" >&2; exit 2 ;;
esac
printf '%s\n' '{"type":"thread.started","thread_id":"fixture"}'
printf '%s\n' '{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"function f(a){ return a.reduce((s,x)=>s+x,0); }"}}'
printf '%s\n' '{"type":"turn.completed","usage":{"input_tokens":100,"output_tokens":17,"reasoning_output_tokens":3}}'
EOF
chmod +x "$T/codex-model.sh"
cat > "$T/models-codex.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
codexfix	CodexFixture	$T/codex-model.sh	codex-exec {prompt}	codex-jsonl	usage.output_tokens	5
EOF
r=$(MODEL_REGISTRY="$T/models-codex.tsv" MODEL_ID=codexfix sh bench/llm/drive-js.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV")
chk model-registry-codex '"model_id":"codexfix","model":"CodexFixture","arm":"js".*"outcome":"pass","rounds":1.*"tokens":17' "$r"

cat > "$T/models2.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
alpha	AlphaJS	$T/js.sh	-p {prompt} --output-format json	raw		5
beta	BetaJS	$T/js.sh	-p {prompt} --output-format json	raw		5
EOF
: > "$T/multi-model.jsonl"
MODEL_REGISTRY="$T/models2.tsv" MODEL_ID=alpha BENCH_TRIAL=1 BENCH_TASK_ORDER=7 BENCH_K=2 BENCH_SEED=fixture-seed \
  sh bench/llm/drive-js.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" >> "$T/multi-model.jsonl"
MODEL_REGISTRY="$T/models2.tsv" MODEL_ID=beta BENCH_TRIAL=1 BENCH_TASK_ORDER=7 BENCH_K=2 BENCH_SEED=fixture-seed \
  sh bench/llm/drive-js.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" >> "$T/multi-model.jsonl"
multi=$(cat "$T/multi-model.jsonl")
chk multi-model-alpha '"model_id":"alpha","model":"AlphaJS","arm":"js","trial_id":"fixture-seed:alpha:js:1:1","trial":1,"task_order":7,"k_trials":2,"order_seed":"fixture-seed"' "$multi"
chk multi-model-beta '"model_id":"beta","model":"BetaJS","arm":"js","trial_id":"fixture-seed:beta:js:1:1","trial":1,"task_order":7,"k_trials":2,"order_seed":"fixture-seed"' "$multi"
rep=$(sh bench/llm/report.sh "$T/multi-model.jsonl")
chk multi-report-alpha '| AlphaJS | JavaScript | 1 | 1 | 100% | 100% | 100% | 0 |' "$rep"
chk multi-report-beta '| BetaJS | JavaScript | 1 | 1 | 100% | 100% | 100% | 0 |' "$rep"

cat > "$T/hbprose.sh" <<EOF
#!/bin/sh
cat <<'OUT'
The bug: the loop needs to add each cell.
: ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop ;
OUT
EOF
chmod +x "$T/hbprose.sh"
r=$(CLAUDE="$T/hbprose.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" a); chk habu-extract-prose '"outcome":"pass","rounds":1' "$r"

mkstub "$T/forth-good.sh" 'echo ": SQUARE ( i64 -- i64 ) dup * ;"'
r=$(CLAUDE="$T/forth-good.sh" sh bench/llm/drive-forth.sh 1 SQUARE "i64 -- i64" arithmetic "7 -> 49; -3 -> 9" "Define SQUARE with the checked Forth stack effect." 2)
chk forth-driver-pass '"arm":"habu-forth".*"outcome":"pass","rounds":1' "$r"
chk forth-driver-family '"task_family":"arithmetic"' "$r"
chk forth-driver-empty-repairs '"repair_class_stats":\[\]' "$r"
chk forth-driver-replay '"prompt_sha256":"[0-9a-f][0-9a-f]*".*"final_bundle_sha256":"[0-9a-f][0-9a-f]*"' "$r"

cat > "$T/forth-repair.sh" <<EOF
#!/bin/sh
if [ -f "$T/forth-repair.seen" ]; then
  echo ": CUBE ( i64 -- i64 ) dup dup * * ;"
else
  touch "$T/forth-repair.seen"
  echo ": CUBE ( i64 -- i64 ) dup ;"
fi
EOF
chmod +x "$T/forth-repair.sh"
r=$(CLAUDE="$T/forth-repair.sh" sh bench/llm/drive-forth.sh 2 CUBE "i64 -- i64" arithmetic "3 -> 27" "Define CUBE with the checked Forth stack effect." 3)
chk forth-driver-repair '"outcome":"pass","rounds":2.*"first_pass_checker":"rejected","first_pass_tests":false,"tests_passed":true' "$r"
chk forth-driver-repair-stats '"diagnostic_count":1.*"repair_class_stats":\[{"repair_class":"remove_producer","diagnostic_count":1,"repair_success":true,"repair_iterations":1,"token_delta":0}\]' "$r"

cat > "$T/forth-rawdiag.sh" <<EOF
#!/bin/sh
if [ -f "$T/forth-rawdiag.seen" ]; then
  echo ": POW ( i64 i64 -- i64 ) {: b e :} 1 e 0 ?do b * loop ;"
else
  touch "$T/forth-rawdiag.seen"
  echo ": POW ( i64 i64 -- i64 ) {: b e :} e 0= if 1 else b e 1 - POW b * then ;"
fi
EOF
chmod +x "$T/forth-rawdiag.sh"
r=$(CLAUDE="$T/forth-rawdiag.sh" sh bench/llm/drive-forth.sh 19 POW "i64 i64 -- i64" loop "2 3 -> 8; 5 0 -> 1" "Define POW with the checked Forth stack effect." 3)
chk forth-driver-rawdiag-repair '"outcome":"pass","rounds":2' "$r"
chk forth-driver-rawdiag-code 'E-UNDEFINED' "$r"
chk forth-driver-rawdiag-class '"repair_class":"unknown_rejection"' "$r"
chk forth-driver-rawdiag-stats '"repair_class_stats":\[{"repair_class":"unknown_rejection","diagnostic_count":1,"repair_success":true,"repair_iterations":1,"token_delta":0}\]' "$r"

cat > "$T/forth-raw-feedback.sh" <<EOF
#!/bin/sh
if [ -f "$T/forth-raw-feedback.seen" ]; then
  echo ": CUBE ( i64 -- i64 ) dup dup * * ;"
else
  touch "$T/forth-raw-feedback.seen"
  echo ": CUBE ( i64 -- i64 ) dup ;"
fi
EOF
chmod +x "$T/forth-raw-feedback.sh"
r=$(BENCH_FORTH_FEEDBACK=raw CLAUDE="$T/forth-raw-feedback.sh" sh bench/llm/drive-forth.sh 2 CUBE "i64 -- i64" arithmetic "3 -> 27" "Define CUBE with the checked Forth stack effect." 3)
chk forth-driver-raw-feedback '"arm":"habu-forth-raw".*"outcome":"pass","rounds":2' "$r"
chk forth-driver-raw-feedback-prompt 'Raw checker diagnostics' "$r"

cat > "$T/forth-blind-feedback.sh" <<EOF
#!/bin/sh
if [ -f "$T/forth-blind-feedback.seen" ]; then
  echo ": CUBE ( i64 -- i64 ) dup dup * * ;"
else
  touch "$T/forth-blind-feedback.seen"
  echo ": CUBE ( i64 -- i64 ) dup ;"
fi
EOF
chmod +x "$T/forth-blind-feedback.sh"
r=$(BENCH_FORTH_FEEDBACK=blind CLAUDE="$T/forth-blind-feedback.sh" sh bench/llm/drive-forth.sh 2 CUBE "i64 -- i64" arithmetic "3 -> 27" "Define CUBE with the checked Forth stack effect." 3)
chk forth-driver-blind-feedback '"arm":"habu-forth-blind".*"outcome":"pass","rounds":2' "$r"
chk forth-driver-blind-feedback-prompt 'attempt did not certify' "$r"
if printf '%s' "$r" | grep -Eq 'Use this repair packet|Raw checker diagnostics'; then
  echo "FAIL: forth-driver-blind-feedback-detail -> prompt exposed diagnostic detail"
  fails=$((fails+1))
else
  echo "ok: forth-driver-blind-feedback-detail"
fi

cat > "$T/forth-codex-last.sh" <<'EOF'
#!/bin/sh
out=
while [ "$#" -gt 0 ]; do
  if [ "$1" = "-o" ]; then out=$2; shift 2; continue; fi
  shift
done
[ -n "$out" ] || exit 2
printf '%s\n' ': SQUARE ( i64 -- i64 ) dup * ;' > "$out"
printf '%s\n' 'not-json'
EOF
chmod +x "$T/forth-codex-last.sh"
cat > "$T/models-forth-codex-last.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
codexlast	CodexLast	$T/forth-codex-last.sh	codex-exec {prompt}	codex-jsonl	usage.output_tokens	5
EOF
r=$(MODEL_REGISTRY="$T/models-forth-codex-last.tsv" MODEL_ID=codexlast sh bench/llm/drive-forth.sh 1 SQUARE "i64 -- i64" arithmetic "7 -> 49; -3 -> 9" "Define SQUARE with the checked Forth stack effect." 2)
chk forth-driver-codex-last '"model_id":"codexlast","model":"CodexLast","arm":"habu-forth".*"outcome":"pass","rounds":1.*"tokens":0' "$r"

cat > "$T/models-forth.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
forthfix	ForthFixture	$T/forth-good.sh	{prompt}	raw		5
EOF
MODEL_REGISTRY="$T/models-forth.tsv" MODEL_ID=forthfix BENCH_TASK_IDS=1 BENCH_RESULTS="$T/forth-results.md" \
  sh bench/llm/run-forth-bench.sh 1 "$T/forth-run.jsonl" >/dev/null
[ "$(wc -l < "$T/forth-run.jsonl" | tr -d ' ')" = 3 ] && echo "ok: forth-runner-row-count" || {
  echo "FAIL: forth-runner-row-count"
  fails=$((fails+1))
}
forth_default_rows=$(cat "$T/forth-run.jsonl")
chk forth-runner-default-repair '"arm":"habu-forth"' "$forth_default_rows"
chk forth-runner-default-raw '"arm":"habu-forth-raw"' "$forth_default_rows"
chk forth-runner-default-blind '"arm":"habu-forth-blind"' "$forth_default_rows"
chk forth-runner-report 'category arithmetic rows=3 certified=3 tests=3' "$(cat "$T/forth-results.md")"

cat > "$T/forth-kill-driver.sh" <<'EOF'
#!/bin/sh
timeout_pid=$PPID
driver_pid=$(ps -o ppid= -p "$timeout_pid" | tr -d ' ')
[ -n "$driver_pid" ] && kill -TERM "$driver_pid" 2>/dev/null
exit 1
EOF
chmod +x "$T/forth-kill-driver.sh"
cat > "$T/models-forth-kill.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
forthkill	ForthKill	$T/forth-kill-driver.sh	{prompt}	raw		5
EOF
set +e
MODEL_REGISTRY="$T/models-forth-kill.tsv" MODEL_ID=forthkill BENCH_TASK_IDS=1 BENCH_RESULTS="$T/forth-missing.md" \
  sh bench/llm/run-forth-bench.sh 1 "$T/forth-missing.jsonl" >"$T/forth-missing.out" 2>"$T/forth-missing.err"
missing_rc=$?
set -e
[ "$missing_rc" -ne 0 ] && grep -q 'missing row task=1 model=forthkill arm=habu-forth trial=1' "$T/forth-missing.err" && echo "ok: forth-runner-missing-row" || {
  echo "FAIL: forth-runner-missing-row -> rc=$missing_rc err=$(cat "$T/forth-missing.err")"
  fails=$((fails+1))
}

MODEL_REGISTRY="$T/models-forth.tsv" MODEL_ID=forthfix BENCH_TASK_IDS=1 BENCH_FORTH_MODES="repair raw" BENCH_SEED=forth-modes-resume BENCH_RESULTS="$T/forth-modes.md" \
  sh bench/llm/run-forth-bench.sh 1 "$T/forth-modes.jsonl" >/dev/null
[ "$(wc -l < "$T/forth-modes.jsonl" | tr -d ' ')" = 2 ] && echo "ok: forth-runner-modes-row-count" || {
  echo "FAIL: forth-runner-modes-row-count"
  fails=$((fails+1))
}
modes_rows=$(cat "$T/forth-modes.jsonl")
chk forth-runner-modes-repair '"arm":"habu-forth"' "$modes_rows"
chk forth-runner-modes-raw '"arm":"habu-forth-raw"' "$modes_rows"
chk forth-runner-modes-report 'rows=2 certified=2 first_tests=2 tests=2' "$(cat "$T/forth-modes.md")"

sed -n '1p' "$T/forth-modes.jsonl" > "$T/forth-modes-partial.jsonl"
MODEL_REGISTRY="$T/models-forth.tsv" MODEL_ID=forthfix BENCH_TASK_IDS=1 BENCH_FORTH_MODES="repair raw" BENCH_SEED=forth-modes-resume BENCH_RESUME=1 BENCH_RESULTS="$T/forth-modes-resume.md" \
  sh bench/llm/run-forth-bench.sh 1 "$T/forth-modes-partial.jsonl" >/dev/null
[ "$(wc -l < "$T/forth-modes-partial.jsonl" | tr -d ' ')" = 2 ] && echo "ok: forth-runner-modes-resume-row-count" || {
  echo "FAIL: forth-runner-modes-resume-row-count"
  fails=$((fails+1))
}

MODEL_REGISTRY="$T/models-forth.tsv" MODEL_ID=forthfix BENCH_TASK_IDS=1 BENCH_SEED=forth-resume BENCH_RESULTS="$T/forth-resume-full.md" \
  sh bench/llm/run-forth-bench.sh 2 "$T/forth-resume-full.jsonl" >/dev/null
sed -n '1p' "$T/forth-resume-full.jsonl" > "$T/forth-resume-partial.jsonl"
MODEL_REGISTRY="$T/models-forth.tsv" MODEL_ID=forthfix BENCH_TASK_IDS=1 BENCH_SEED=forth-resume BENCH_RESUME=1 BENCH_RESULTS="$T/forth-resume.md" \
  sh bench/llm/run-forth-bench.sh 2 "$T/forth-resume-partial.jsonl" >/dev/null
[ "$(wc -l < "$T/forth-resume-partial.jsonl" | tr -d ' ')" = 6 ] && echo "ok: forth-runner-resume-row-count" || {
  echo "FAIL: forth-runner-resume-row-count"
  fails=$((fails+1))
}
chk forth-runner-resume-report 'rows=6 certified=6 first_tests=6 tests=6' "$(cat "$T/forth-resume.md")"

cat > "$T/expanded-native-tasks.tsv" <<'EOF'
id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
1	SQUARE	(i64 -- i64)	arithmetic	7 -> 49; -3 -> 9	forth	stack	Define SQUARE with the checked Forth stack effect.	-	arithmetic,forth	-	-
EOF
expanded_native="$T/run-expanded-native.f"
cat lib/errors.f lib/string.f lib/fs.f lib/process.f lib/process-argv.f lib/time.f lib/date.f lib/argv.f \
  bench/llm/manifest.f bench/llm/run-expanded-bench.f > "$expanded_native"
MODEL_REGISTRY="$T/models-forth.tsv" MODEL_ID=forthfix BENCH_TASKS="$T/expanded-native-tasks.tsv" BENCH_SEED=expanded-native BENCH_RESULTS="$T/expanded-native.md" \
  bin/hb "$expanded_native" 1 "$T/expanded-native.jsonl" >/dev/null
[ "$(wc -l < "$T/expanded-native.jsonl" | tr -d ' ')" = 3 ] && echo "ok: expanded-native-row-count" || {
  echo "FAIL: expanded-native-row-count"
  fails=$((fails+1))
}
expanded_native_rows=$(cat "$T/expanded-native.jsonl")
chk expanded-native-repair '"arm":"habu-forth"' "$expanded_native_rows"
chk expanded-native-raw '"arm":"habu-forth-raw"' "$expanded_native_rows"
chk expanded-native-blind '"arm":"habu-forth-blind"' "$expanded_native_rows"
chk expanded-native-report 'rows=3 certified=3 first_tests=3 tests=3' "$(cat "$T/expanded-native.md")"
sed -n '1p' "$T/expanded-native.jsonl" > "$T/expanded-native-partial.jsonl"
MODEL_REGISTRY="$T/models-forth.tsv" MODEL_ID=forthfix BENCH_TASKS="$T/expanded-native-tasks.tsv" BENCH_SEED=expanded-native BENCH_RESUME=1 BENCH_RESULTS="$T/expanded-native-resume.md" \
  bin/hb "$expanded_native" 1 "$T/expanded-native-partial.jsonl" >/dev/null
[ "$(wc -l < "$T/expanded-native-partial.jsonl" | tr -d ' ')" = 3 ] && echo "ok: expanded-native-resume-row-count" || {
  echo "FAIL: expanded-native-resume-row-count"
  fails=$((fails+1))
}

cat > "$T/expanded-native-array-tasks.tsv" <<'EOF'
id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
46	ARR-SUM	(ptr a n -- i64)	arrays	[3 1 4] -> 8	array	as	Return the sum.	[3 1 4] -> 8; [5] -> 5	array	function f(a) -> number	fn f(a: &[i64]) -> i64
EOF
mkstub "$T/expanded-js.sh" 'echo "function f(a){ return a.reduce((s,x)=>s+x,0); }"'
cat > "$T/models-expanded-js.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
jsfix	JSFixture	$T/expanded-js.sh	{prompt}	raw		5
EOF
MODEL_REGISTRY="$T/models-expanded-js.tsv" MODEL_ID=jsfix BENCH_TASKS="$T/expanded-native-array-tasks.tsv" BENCH_ARRAY_ARMS=js BENCH_SEED=expanded-array BENCH_RESULTS="$T/expanded-array.md" \
  bin/hb "$expanded_native" 1 "$T/expanded-array.jsonl" >/dev/null
[ "$(wc -l < "$T/expanded-array.jsonl" | tr -d ' ')" = 1 ] && echo "ok: expanded-native-array-row-count" || {
  echo "FAIL: expanded-native-array-row-count"
  fails=$((fails+1))
}
expanded_array_rows=$(cat "$T/expanded-array.jsonl")
chk expanded-native-array-arm '"arm":"js"' "$expanded_array_rows"
chk expanded-native-array-report 'category arrays rows=1 certified=1 tests=1' "$(cat "$T/expanded-array.md")"

# --- conv=aa : REVERSE (array -> array, in place) ---
mkstub "$T/hb2.sh" 'echo ": REVERSE ( ptr a n -- ) {: arr:ptr len :} len 2 / 0 ?do i cells arr + @ len 1 - i - cells arr + @ i cells arr + ! len 1 - i - cells arr + ! loop ;"'
mkstub "$T/hbl2.sh" 'echo ": REVERSE ( ptr a n -- ) {: arr:ptr len :} len 2 / 0 ?do arr len i len i MIRROR-INDEX A-SWAP loop ;"'
mkstub "$T/hbstd2.sh" 'echo ": REVERSE ( ptr a n -- ) {: arr:ptr len :} len 2 / 0 ?do arr len i len i MIRROR-INDEX A-SWAP loop ;"'
mkstub "$T/hbsk2.sh" 'echo "len 2 / 0 ?do i cells arr + @ len 1 - i - cells arr + @ i cells arr + ! len 1 - i - cells arr + ! loop"'
mkstub "$T/js2.sh" 'echo "function f(a){ return a.slice().reverse(); }"'
mkstub "$T/py2.sh" 'echo "def f(a): return list(reversed(a))"'
mkstub "$T/ts2.sh" 'echo "function f(a: number[]): number[] { return a.slice().reverse(); }"'
mkstub "$T/rs2.sh" 'echo "fn f(a: &[i64]) -> Vec<i64> { a.iter().rev().cloned().collect() }"'
MV="[3 1 4 1 5] -> [5 1 4 1 3]; [1 2] -> [2 1]"
r=$(CLAUDE="$T/hb2.sh" sh bench/llm/drive-habu.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV" a); chk habu-aa '"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbl2.sh" sh bench/llm/drive-habu.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV" lib); chk habu-lib-aa '"arm":"habu-lib","trial_id":"manifest:claude:habu-lib:6:0","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbstd2.sh" sh bench/llm/drive-habu.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV" stdlib); chk habu-stdlib-aa '"arm":"habu-stdlib","trial_id":"manifest:claude:habu-stdlib:6:0","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbsk2.sh" sh bench/llm/drive-habu.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV" skeleton); chk habu-skeleton-aa '"arm":"habu-skeleton","trial_id":"manifest:claude:habu-skeleton:6:0","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/js2.sh" sh bench/llm/drive-js.sh   6 REVERSE "ptr a n --" "reverse" aa "$MV");   chk js-aa   '"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/py2.sh" sh bench/llm/drive-python.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV"); chk python-aa '"arm":"python".*"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/ts2.sh" sh bench/llm/drive-ts.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV"); chk ts-aa '"arm":"ts".*"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/rs2.sh" sh bench/llm/drive-rust.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV");   chk rust-aa '"outcome":"pass","rounds":1' "$r"

# --- habu repair loop: reject (untyped ptr) -> diagnostic -> typed fix ---
cat > "$T/hbr.sh" <<EOF
#!/bin/sh
c="$T/n"; n=\$(cat "\$c" 2>/dev/null || echo 0); n=\$((n+1)); echo "\$n" > "\$c"
if [ "\$n" -eq 1 ]; then echo ': ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop dup ;'
else echo ': ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop ;'; fi
EOF
chmod +x "$T/hbr.sh"
r=$(CLAUDE="$T/hbr.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" a); chk habu-repair '"outcome":"pass","rounds":2' "$r"
chk habu-repair-packet 'habu_repair_packet' "$r"
chk habu-repair-class 'remove_producer' "$r"

cat > "$T/pyr.sh" <<EOF
#!/bin/sh
c="$T/pyn"; n=\$(cat "\$c" 2>/dev/null || echo 0); n=\$((n+1)); echo "\$n" > "\$c"
if [ "\$n" -eq 1 ]; then echo 'def f(a): return 0'
else echo 'def f(a): return sum(a)'; fi
EOF
chmod +x "$T/pyr.sh"
r=$(CLAUDE="$T/pyr.sh" sh bench/llm/drive-python.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV"); chk python-repair '"arm":"python".*"outcome":"pass","rounds":2' "$r"

cat > "$T/tsr.sh" <<EOF
#!/bin/sh
c="$T/tsn"; n=\$(cat "\$c" 2>/dev/null || echo 0); n=\$((n+1)); echo "\$n" > "\$c"
if [ "\$n" -eq 1 ]; then echo 'function f(a: number[]): number { return 0; }'
else echo 'function f(a: number[]): number { return a.reduce((s, x) => s + x, 0); }'; fi
EOF
chmod +x "$T/tsr.sh"
r=$(CLAUDE="$T/tsr.sh" sh bench/llm/drive-ts.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV"); chk ts-repair '"arm":"ts".*"outcome":"pass","rounds":2' "$r"

if [ "$manifest_ready" = 1 ]; then
  cat > "$T/canon-model.sh" <<'EOF'
#!/bin/sh
case "$1" in
  *"JavaScript function"*) echo 'function f(a){ return a.reduce((s,x)=>s+x,0); }' ;;
  *"Python function"*) echo 'def f(a): return sum(a)' ;;
  *"TypeScript function"*) echo 'function f(a: number[]): number { return a.reduce((s, x) => s + x, 0); }' ;;
  *"Rust function"*) echo 'fn f(a: &[i64]) -> i64 { a.iter().sum() }' ;;
  *"CANON-SUM"*) echo ': CANON-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop ;' ;;
  *) echo "unexpected prompt" >&2; exit 2 ;;
esac
EOF
  chmod +x "$T/canon-model.sh"
  cat > "$T/models.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
fixture	CanonManifest	$T/canon-model.sh	{prompt}	raw		5
EOF
  cat > "$T/canon-tasks.tsv" <<'EOF'
id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
99	CANON-SUM	(ptr a n -- i64)	arrays	[3 1 4] -> 8; [5] -> 5	array	as	Return the sum of all elements of the array.	[3 1 4] -> 8; [5] -> 5	array,scalar	function f(a) -> number	fn f(a: &[i64]) -> i64
EOF
  MODEL_REGISTRY="$T/models.tsv" MODEL_ID=fixture BENCH_TASKS="$T/canon-tasks.tsv" BENCH_RESULTS="$T/canon-results.md" \
    sh bench/llm/run-bench.sh 1 "$T/canon-run.jsonl" >"$T/canon-run.out" 2>"$T/canon-run.err"
  run_rows=$(wc -l < "$T/canon-run.jsonl" | tr -d ' ')
  [ "$run_rows" = 8 ] && echo "ok: run-bench-canonical-row-count" || {
    echo "FAIL: run-bench-canonical-row-count -> $run_rows"
    cat "$T/canon-run.err"
    fails=$((fails+1))
  }
  canon_rows=$(grep -c '"name":"CANON-SUM"' "$T/canon-run.jsonl" || true)
  [ "$canon_rows" = 8 ] && echo "ok: run-bench-canonical-task" || {
    echo "FAIL: run-bench-canonical-task -> $canon_rows"
    cat "$T/canon-run.jsonl"
    fails=$((fails+1))
  }
  if grep -q '"name":"ARR-SUM"' "$T/canon-run.jsonl"; then
    echo "FAIL: run-bench-canonical-no-legacy -> found ARR-SUM task row"
    fails=$((fails+1))
  else
    echo "ok: run-bench-canonical-no-legacy"
  fi
  cat > "$T/models-filter.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
alpha	Alpha	$T/canon-model.sh	{prompt}	raw		5
beta	Beta	$T/canon-model.sh	{prompt}	raw		5
EOF
  MODEL_REGISTRY="$T/models-filter.tsv" MODEL_ID=beta BENCH_TASKS="$T/canon-tasks.tsv" BENCH_RESULTS="$T/filter-results.md" \
    sh bench/llm/run-bench.sh 1 "$T/filter-run.jsonl" >"$T/filter-run.out" 2>"$T/filter-run.err"
  filter_rows=$(wc -l < "$T/filter-run.jsonl" | tr -d ' ')
  filter_beta=$(grep -c '"model_id":"beta"' "$T/filter-run.jsonl" || true)
  filter_alpha=$(grep -c '"model_id":"alpha"' "$T/filter-run.jsonl" || true)
  [ "$filter_rows" = 8 ] && [ "$filter_beta" = 8 ] && [ "$filter_alpha" = 0 ] && echo "ok: run-bench-model-filter" || {
    echo "FAIL: run-bench-model-filter -> rows=$filter_rows beta=$filter_beta alpha=$filter_alpha"
    cat "$T/filter-run.err"
    fails=$((fails+1))
  }
fi

echo "----"
if [ "$fails" -eq 0 ]; then echo "PASS: array drivers (as + aa, 8 arms + repair loops)"
else echo "FAIL: bench-test ($fails case(s))"; exit 1; fi
