#!/bin/sh
# Focused tests for the Habu aot-call-report tool.
set -eu
cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}
[ -x "$HB" ] || { echo "aot-call-report-test: $HB missing or not executable"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-aot-call-report.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

sed '$d' tools/aot-call-report.f > "$T/aot-call-report-lib.f"
cat "$T/aot-call-report-lib.f" tools/aot-call-report-test.f | "$HB"

cat "$T/aot-call-report-lib.f" > "$T/make-fixture.f"
cat >> "$T/make-fixture.f" <<'EOF'
0 set-check
create TBUF 64 allot
create TPATH 64 allot
variable TFD
: T-COPY {: a dst u :} ( a dst u -- )
   0 begin dup u < while dup a + c@ over dst + c! 1+ repeat drop ;
: T-PATH! {: a u :} ( a u -- )
   a TPATH u T-COPY
   0 TPATH u + c! ;
: T-W32! {: w off :} ( w off -- )
   w TBUF off + c!
   w 8 rshift TBUF off 1+ + c!
   w 16 rshift TBUF off 2 + + c!
   w 24 rshift TBUF off 3 + + c! ;
: T-WRITE {: a u n :} ( a u n -- )
   a u T-PATH!
   TPATH 1537 493 open TFD !
   TFD @ TBUF n write drop
   TFD @ close ;
: GO ( -- )
   NOP-INSTR 0 T-W32!
   NOP-INSTR 4 T-W32!
   NOP-INSTR 8 T-W32!
   $94000005 12 T-W32!
   s" /tmp/habu-aot-report-cli.bin" 16 T-WRITE ;
GO
EOF
"$HB" < "$T/make-fixture.f"

"$HB" tools/aot-call-report.f /tmp/habu-aot-report-cli.bin < /dev/null > "$T/report.json"
grep -Fq '"patched_call_stencils":1' "$T/report.json"
grep -Fq '"padding_bytes":12' "$T/report.json"
grep -Fq '"direct_bl_sites":[12]' "$T/report.json"
grep -Fq '"sites":[0]' "$T/report.json"

set +e
"$HB" tools/aot-call-report.f < /dev/null > "$T/noarg.out" 2> "$T/noarg.err"
rc=$?
set -e
[ "$rc" -eq 64 ] || { echo "aot-call-report-test: no-arg rc $rc, want 64"; exit 1; }
grep -q 'usage: tools/aot-call-report.f binary' "$T/noarg.err"

echo "aot-call-report-test: ok"
