# lib.sh — shared helpers for the array-algorithm benchmark. POSIX sh; source it.
# Tasks operate on an INTEGER ARRAY. Two calling conventions (conv):
#   as : (array) -> scalar         e.g. ARR-SUM [3 1 4] -> 8
#   aa : (array) -> array          e.g. REVERSE [3 1 4] -> [4 1 3]   (habu: in place)
# Vectors use [..] for arrays. Single source of truth = bench-tasks.tsv; each arm's
# test harness is GENERATED from the same vectors.
#   habu: builds the array in memory (here , ,) and calls ( ptr n -- ... );
#   JS:   f(arr)  returns a number (as) or array (aa);
#   Rust: fn f(a:&[i64]) -> i64 (as) or -> Vec<i64> (aa).

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

js_ret()   { [ "$1" = as ] && printf 'a single integer' || printf 'a NEW array of integers'; }
rust_ret() { [ "$1" = as ] && printf 'i64' || printf 'Vec<i64>'; }

case_list() {
  printf '%s\n' "$1" | tr ';' '\n' | while IFS= read -r p; do
    [ -n "$p" ] || continue
    printf '  %s\n' "$(printf '%s' "$p" | xargs)"
  done
}

# emit_row: id name model arm outcome rounds tokens wall_ms
emit_row() {
  fp=false; [ "$5" = pass ] && [ "$6" -eq 1 ] && fp=true
  printf '{"task_id":%s,"name":"%s","model_id":"%s","model":"%s","arm":"%s","trial":%s,"task_order":%s,"k_trials":%s,"order_seed":"%s","outcome":"%s","rounds":%s,"first_pass":%s,"tokens":%s,"wall_ms":%s}\n' \
    "$1" "$2" "${MODEL_ID:-$3}" "$3" "$4" \
    "${BENCH_TRIAL:-0}" "${BENCH_TASK_ORDER:-0}" "${BENCH_K:-0}" "${BENCH_SEED:-manifest}" \
    "$5" "$6" "$fp" "$7" "$8"
}
