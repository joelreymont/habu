#!/bin/sh
# perf.sh - repeatable LLM-development latency benchmarks.
# Default mode is quick and non-gating. Use --full for rebuild/AOT timings.
set -eu

cd "$(dirname "$0")/../.."

JSON=0
FULL=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    --json) JSON=1; shift ;;
    --full) FULL=1; shift ;;
    --) shift; break ;;
    *) echo "usage: bench/llm/perf.sh [--json] [--full]"; exit 64 ;;
  esac
done
[ "$#" -eq 0 ] || { echo "usage: bench/llm/perf.sh [--json] [--full]"; exit 64; }

[ -x bin/hb ] || { echo "llm-perf: no bin/hb - run tools/build.sh"; exit 69; }

CLEAN_T=0
if [ -n "${HB_TMP:-}" ]; then
  T=$HB_TMP/llm-perf
else
  T=$(mktemp -d "${TMPDIR:-/tmp}/hb-llm-perf.XXXXXX")
  CLEAN_T=1
fi
mkdir -p "$T"
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

NOW=$T/now.f
printf '%s\n' 'mono-ns .' > "$NOW"

now_ns() {
  bin/hb "$NOW" | tr -d '[:space:]'
}

json_string() {
  printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g'
}

if [ "$JSON" = 1 ]; then
  if [ "$FULL" = 1 ]; then full_json=true; else full_json=false; fi
  printf '{"schema_version":1,"bench":"llm-perf","full":%s,"results":[' "$full_json"
  FIRST=1
else
  if [ "$FULL" = 1 ]; then
    echo "llm-perf: mode=full"
  else
    echo "llm-perf: mode=quick"
  fi
fi

record() {
  name=$1
  ms=$2
  if [ "$JSON" = 1 ]; then
    [ "$FIRST" = 1 ] || printf ','
    FIRST=0
    printf '{"name":"%s","wall_ms":%s}' "$(json_string "$name")" "$ms"
  else
    printf 'llm-perf: %-24s %8s ms\n' "$name" "$ms"
  fi
}

measure() {
  name=$1
  shift
  start=$(now_ns)
  set +e
  "$@" >"$T/$name.out" 2>"$T/$name.err"
  rc=$?
  set -e
  end=$(now_ns)
  ms=$(( (end - start + 999999) / 1000000 ))
  if [ "$rc" -ne 0 ]; then
    [ "$JSON" = 1 ] && printf ']}\n'
    echo "llm-perf: $name failed rc=$rc" >&2
    tail -20 "$T/$name.err" >&2
    exit "$rc"
  fi
  record "$name" "$ms"
}

VALIDATOR=$T/validate-results.f
cat tools/date.f tools/lint/lib.f tools/json.f tools/argv.f bench/llm/validate-results.f > "$VALIDATOR"

measure check_solutions ./tools/check.sh bench/llm/solutions.f
measure functional_tests sh -c 'cat bench/llm/solutions.f bench/llm/tests.f | bin/hb'
measure metric_validator bin/hb "$VALIDATOR"
measure prop_smoke_250 sh -c 'bin/hb 123 250 < test/prop-test.f'
measure microbench_smoke ./tools/bench.sh --smoke

if [ "$FULL" = 1 ]; then
  measure self_rebuild ./tools/build.sh
  PERF_SRC=$T/perf-main.f
  PERF_BIN=$T/perf-main
  {
    printf '%s\n' ': FIB ( n -- n ) dup 2 < if exit then dup 1 - recurse swap 2 - recurse + ;'
    printf '%s\n' ': MAIN ( -- ) 10 FIB . cr ;'
  } > "$PERF_SRC"
  measure hb_build_aot ./tools/hb-build.sh "$PERF_SRC" -o "$PERF_BIN"
  measure aot_runtime "$PERF_BIN"
fi

if [ "$JSON" = 1 ]; then
  printf ']}\n'
fi
