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

printf '{"result":": X ;","modelUsage":{"claude-opus-4":{"outputTokens":7}}}\n' > "$T/resp.json"
rt=$(node bench/llm/parse-resp.js "$T/resp.json" "$T/resp.txt")
[ "$rt" = 7 ] && [ "$(cat "$T/resp.txt")" = ': X ;' ] && echo "ok: parse-resp-modelUsage" || {
  echo "FAIL: parse-resp-modelUsage -> tokens=$rt text=$(cat "$T/resp.txt")"
  fails=$((fails+1))
}

# --- conv=as : ARR-SUM (array -> scalar) ---
mkstub "$T/hb.sh" 'echo ": ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop ;"'
mkstub "$T/hbl.sh" 'echo ": ARR-SUM ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do arr i A@ + loop ;"'
mkstub "$T/js.sh" 'echo "function f(a){ return a.reduce((s,x)=>s+x,0); }"'
mkstub "$T/rs.sh" 'echo "fn f(a: &[i64]) -> i64 { a.iter().sum() }"'
SV="[3 1 4] -> 8; [5] -> 5"
r=$(CLAUDE="$T/hb.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" a); chk habu-as '"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/hbl.sh" sh bench/llm/drive-habu.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV" lib); chk habu-lib-as '"arm":"habu-lib","outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/js.sh" sh bench/llm/drive-js.sh   1 ARR-SUM "ptr a n -- i64" "sum" as "$SV");   chk js-as   '"outcome":"pass","rounds":1' "$r"
r=$(CLAUDE="$T/rs.sh" sh bench/llm/drive-rust.sh 1 ARR-SUM "ptr a n -- i64" "sum" as "$SV");   chk rust-as '"outcome":"pass","rounds":1' "$r"

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
r=$(CLAUDE="$T/hbl2.sh" sh bench/llm/drive-habu.sh 6 REVERSE "ptr a n --" "reverse" aa "$MV" lib); chk habu-lib-aa '"arm":"habu-lib","outcome":"pass","rounds":1' "$r"
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

echo "----"
if [ "$fails" -eq 0 ]; then echo "PASS: array drivers (as + aa, 4 arms + habu repair)"
else echo "FAIL: bench-test ($fails case(s))"; exit 1; fi
