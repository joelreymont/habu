#!/bin/sh
# expanded-report.sh RESULT.jsonl — summarize expanded live rows with the native
# validator. This is intentionally separate from report.sh, which is the
# array-arm comparison report.
set -e
cd "$(dirname "$0")/../.."

OUT=${1:-bench/llm/results/run-expanded.jsonl}
TITLE=${BENCH_EXPANDED_TITLE:-Expanded Habu Forth Live Benchmark}

[ -f "$OUT" ] || { echo "expanded-report: missing result file: $OUT" >&2; exit 66; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-expanded-report.XXXXXX")
trap 'rm -rf "$T"' EXIT HUP INT TERM

VALIDATOR=$T/validate-results.f
cat tools/date.f tools/lint/lib.f tools/json.f tools/argv.f bench/llm/validate-results.f > "$VALIDATOR"
bin/hb "$VALIDATOR" "$OUT" > "$T/summary.txt"
bin/hb "$VALIDATOR" --json "$OUT" > "$T/summary.json"

rows=$(wc -l < "$OUT" | tr -d ' ')
generated=$(date -u '+%Y-%m-%dT%H:%M:%SZ')

printf '# %s\n\n' "$TITLE"
printf 'Generated: `%s`\n\n' "$generated"
printf 'Raw evidence: `%s` (%s rows)\n\n' "$OUT" "$rows"
printf 'The raw JSONL rows are validated by `bench/llm/validate-results.f`; replay artifacts are embedded in every row with SHA-256 fields.\n\n'
printf '## Validator Summary\n\n'
printf '```text\n'
cat "$T/summary.txt"
printf '```\n\n'
printf '## JSON Summary\n\n'
printf '```json\n'
cat "$T/summary.json"
printf '```\n'
