#!/bin/sh
# bench-test.sh — deterministic teeth for the array-algorithm drivers using STUB
# models (no real claude, no tokens). Covers both conventions (array->scalar,
# array->array) across raw Habu, library-assisted Habu, JS, and Rust, plus
# Habu's repair loop on a checker rejection.
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
rt=$(node bench/llm/parse-resp.js "$T/resp.json" "$T/resp.txt")
[ "$rt" = 7 ] && [ "$(cat "$T/resp.txt")" = ': X ;' ] && echo "ok: parse-resp-modelUsage" || {
  echo "FAIL: parse-resp-modelUsage -> tokens=$rt text=$(cat "$T/resp.txt")"
  fails=$((fails+1))
}

printf '{"choices":[{"message":{"content":"function f(a){return a.length;}"}}],"usage":{"completion_tokens":11}}\n' > "$T/openai.json"
rt=$(node bench/llm/parse-resp.js "$T/openai.json" "$T/openai.txt" openai-json usage.completion_tokens)
[ "$rt" = 11 ] && [ "$(cat "$T/openai.txt")" = 'function f(a){return a.length;}' ] && echo "ok: parse-resp-openai" || {
  echo "FAIL: parse-resp-openai -> tokens=$rt text=$(cat "$T/openai.txt")"
  fails=$((fails+1))
}

cat > "$T/report.jsonl" <<'EOF'
{"task_id":1,"name":"ZERO-TOK","model":"fixture","arm":"habu-a","outcome":"pass","rounds":1,"first_pass":true,"tokens":0,"wall_ms":10}
{"task_id":1,"name":"ZERO-TOK","model":"fixture","arm":"js","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":10}
EOF
rep=$(node bench/llm/report.js "$T/report.jsonl")
chk report-zero-token-note 'exclude 1 passing row' "$rep"
chk report-zero-token-table '| ZERO-TOK | — | — | 5 | — | — | — |' "$rep"

cat > "$T/report-models.jsonl" <<'EOF'
{"task_id":1,"name":"MREG","model":"alpha","arm":"js","outcome":"pass","rounds":1,"first_pass":true,"tokens":5,"wall_ms":10}
{"task_id":1,"name":"MREG","model":"beta","arm":"js","outcome":"fail","rounds":2,"first_pass":false,"tokens":9,"wall_ms":20}
EOF
rep=$(node bench/llm/report.js "$T/report-models.jsonl")
chk report-model-section '## Per-Model Reliability' "$rep"
chk report-model-alpha '| alpha | JavaScript | 1 | 1 | 100% | 100% | 100% | 0 |' "$rep"
chk report-model-beta '| beta | JavaScript | 1 | 0 | 0% | 0% | 0% | 1 |' "$rep"

# --- conv=as : ARR-SUM (array -> scalar) ---
mkstub "$T/hb.sh" 'echo ": ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop ;"'
mkstub "$T/hbl.sh" 'echo ": ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do arr i A@ + loop ;"'
mkstub "$T/js.sh" 'echo "function f(a){ return a.reduce((s,x)=>s+x,0); }"'
mkstub "$T/rs.sh" 'echo "fn f(a: &[i64]) -> i64 { a.iter().sum() }"'
SV="[3 1 4] -> 8; [5] -> 5"
r=$(CLAUDE="$T/hb.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" a); chk habu-as '"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbl.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" lib); chk habu-lib-as '"arm":"habu-lib","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/js.sh" sh bench/llm/drive-js.sh   1 ARR-SUM "ptr a n -- i64" "sum" as "$SV");   chk js-as   '"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/rs.sh" sh bench/llm/drive-rust.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV");   chk rust-as '"outcome":"pass","rounds":1' "$r"

cat > "$T/models.tsv" <<EOF
id	label	command	args	parser	token_fields	timeout_s
fixture	FixtureJS	$T/js.sh	-p {prompt} --output-format json	raw		5
EOF
r=$(MODEL_REGISTRY="$T/models.tsv" MODEL_ID=fixture sh bench/llm/drive-js.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV")
chk model-registry-label '"model_id":"fixture","model":"FixtureJS","arm":"js","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"

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
chk multi-model-alpha '"model_id":"alpha","model":"AlphaJS","arm":"js","trial":1,"task_order":7,"k_trials":2,"order_seed":"fixture-seed"' "$multi"
chk multi-model-beta '"model_id":"beta","model":"BetaJS","arm":"js","trial":1,"task_order":7,"k_trials":2,"order_seed":"fixture-seed"' "$multi"
rep=$(node bench/llm/report.js "$T/multi-model.jsonl")
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

# --- conv=aa : REVERSE (array -> array, in place) ---
mkstub "$T/hb2.sh" 'echo ": REVERSE ( ptr a n -- ) {: arr:ptr len :} len 2 / 0 ?do i cells arr + @ len 1 - i - cells arr + @ i cells arr + ! len 1 - i - cells arr + ! loop ;"'
mkstub "$T/hbl2.sh" 'echo ": REVERSE ( ptr a n -- ) {: arr:ptr len :} len 2 / 0 ?do arr i len i MIRROR-INDEX A-SWAP loop ;"'
mkstub "$T/js2.sh" 'echo "function f(a){ return a.slice().reverse(); }"'
mkstub "$T/rs2.sh" 'echo "fn f(a: &[i64]) -> Vec<i64> { a.iter().rev().cloned().collect() }"'
MV="[3 1 4 1 5] -> [5 1 4 1 3]; [1 2] -> [2 1]"
r=$(CLAUDE="$T/hb2.sh" sh bench/llm/drive-habu.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV" a); chk habu-aa '"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbl2.sh" sh bench/llm/drive-habu.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV" lib); chk habu-lib-aa '"arm":"habu-lib","trial":0,"task_order":0,"k_trials":0,"order_seed":"manifest","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/js2.sh" sh bench/llm/drive-js.sh   6 REVERSE "ptr a n --" "reverse" aa "$MV");   chk js-aa   '"outcome":"pass","rounds":1' "$r"
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

if [ "$manifest_ready" = 1 ]; then
  cat > "$T/canon-model.sh" <<'EOF'
#!/bin/sh
case "$1" in
  *"JavaScript function"*) echo 'function f(a){ return a.reduce((s,x)=>s+x,0); }' ;;
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
  [ "$run_rows" = 4 ] && echo "ok: run-bench-canonical-row-count" || {
    echo "FAIL: run-bench-canonical-row-count -> $run_rows"
    cat "$T/canon-run.err"
    fails=$((fails+1))
  }
  canon_rows=$(grep -c '"name":"CANON-SUM"' "$T/canon-run.jsonl" || true)
  [ "$canon_rows" = 4 ] && echo "ok: run-bench-canonical-task" || {
    echo "FAIL: run-bench-canonical-task -> $canon_rows"
    cat "$T/canon-run.jsonl"
    fails=$((fails+1))
  }
  if grep -q 'ARR-SUM' "$T/canon-run.jsonl"; then
    echo "FAIL: run-bench-canonical-no-legacy -> found ARR-SUM"
    fails=$((fails+1))
  else
    echo "ok: run-bench-canonical-no-legacy"
  fi
fi

echo "----"
if [ "$fails" -eq 0 ]; then echo "PASS: array drivers (as + aa, 4 arms + habu repair)"
else echo "FAIL: bench-test ($fails case(s))"; exit 1; fi
