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
  printf '{"task_id":%s,"name":"%s","model":"%s","arm":"%s","outcome":"%s","rounds":%s,"first_pass":%s,"tokens":%s,"wall_ms":%s}\n' \
    "$1" "$2" "$3" "$4" "$5" "$6" "$fp" "$7" "$8"
}
