\ maki/cad-test.f - checked tests for the Model CAD REPL command skeleton (cad-0b).

require lib/test.f
require lib/string.f
require maki/report.f
require maki/cad.f

package MAKI

\ ---- render containment helper ---------------------------------------------
variable CT-VA  variable CT-VU
: CT-SAVE ( ptr u8 n -- )  CT-VU ! CT-VA ! ;
: CT-IN ( ptr u8 n -- )  CT-VA @ CT-VU @ 2swap CONTAINS? TTRUE ;
: CT-NOTIN ( ptr u8 n -- )  CT-VA @ CT-VU @ 2swap CONTAINS? TFALSE ;

\ ---- fail-closed probes ----------------------------------------------------
: TRY-NOMODEL ( -- )  MODEL-CLEAR LOWER drop ;
: TRY-BADOP   ( -- )  s" BOGUS" OP-KIND drop ;
: TRY-PROMOTE ( -- )  PROMOTE drop ;

\ all-pass report for the promote success path
: ALL-PASS ( -- report )
   RPT-NEW
   s" " V-PASS G-CERTIFY   RPT-GATE!
   s" " V-PASS G-GOLDEN     RPT-GATE!
   s" " V-PASS G-GRADCHECK  RPT-GATE!
   s" " V-PASS G-PROFILE    RPT-GATE! ;

T-RESET

\ ---- no model defined: every command fails closed --------------------------
' TRY-NOMODEL E-CAD-NOMODEL TTHROWS
MODEL-DEFINED? TFALSE

\ ---- unknown op token fails closed -----------------------------------------
' TRY-BADOP E-CAD-OP TTHROWS

\ known op tokens map (spot-check the fail-closed table's happy path)
s" LINEAR"      OP-KIND OP-LINEAR      T=
s" GELU"        OP-KIND OP-GELU        T=
s" SOFTMAX-ROW" OP-KIND OP-SOFTMAX-ROW T=

\ ---- define a toy composition ----------------------------------------------
MODEL: FFN ( x w1 b1 w2 b2 -- y ) LINEAR GELU LINEAR ;
MODEL-DEFINED? TTRUE

\ ---- LOWER: conservative no-fusion plan (K ops) ----------------------------
LOWER
dup RPT-OPS-BEFORE@   3 T=
dup RPT-OPS-AFTER@    3 T=
dup RPT-REGIONS@      3 T=
dup RPT-MATERIALIZED@ 3 T=
dup RPT-RENDER CT-SAVE  s" report.model: FFN" CT-IN
drop

\ ---- FUSE: one region per op, named split reason ---------------------------
FUSE
dup RPT-SPLIT-COUNT 1 T=
dup RPT-RENDER CT-SAVE  s" fusion.split.0:" CT-IN
drop

\ ---- MEMORY: bytes unknown (need shapes) -----------------------------------
MEMORY
dup RPT-BYTES-KNOWN? TFALSE
dup RPT-RENDER CT-SAVE  s" memory.bytes-before: unknown" CT-IN
drop

\ ---- TILE: single host-reference candidate, selected -----------------------
TILE
dup RPT-CAND-COUNT 1 T=
dup RPT-SELECT@ 0 T=
dup RPT-RENDER CT-SAVE  s" schedule.candidate.0: host-reference-v0" CT-IN
drop

\ ---- CERTIFY: model-level static legality passes ---------------------------
CERTIFY dup G-CERTIFY RPT-GATE-TAG@ V-PASS T= drop

\ ---- device gates: honest not-run on host ----------------------------------
GOLDEN
dup G-GOLDEN RPT-GATE-TAG@ V-NOTRUN T=
dup G-GOLDEN RPT-GATE-REASON@ s" no-device" T$=
drop
GRADCHECK dup G-GRADCHECK RPT-GATE-TAG@ V-NOTRUN T= drop
PROFILE
dup G-PROFILE RPT-GATE-TAG@ V-NOTRUN T=
dup RPT-ROOFLINE@ RC-UNKNOWN T=
drop

\ ---- TUNE: schedule candidate present --------------------------------------
TUNE dup RPT-CAND-COUNT 1 T= drop

\ ---- OPTIMIZE: aggregate report, promotion refused (not thrown) ------------
OPTIMIZE
dup G-CERTIFY RPT-GATE-TAG@ V-PASS   T=
dup G-GOLDEN  RPT-GATE-TAG@ V-NOTRUN T=
dup RPT-RENDER CT-SAVE
s" gate.certify.verdict: pass"    CT-IN
s" gate.golden.verdict: not-run"  CT-IN
s" promote: refused"              CT-IN
drop

\ ---- PROMOTE: refuses via named throw while gates are not all pass ---------
' TRY-PROMOTE E-CAD-GATE TTHROWS

\ ---- promotion gate logic --------------------------------------------------
FULL-REPORT PROMOTE-OK? TFALSE drop
ALL-PASS    PROMOTE-OK? TTRUE  drop

\ ---- promote success path caches the artifact key --------------------------
ALL-PASS PROMOTE-REPORT
dup RPT-CACHE$ s" FFN" T$=
drop

\ ---- EXPLAIN: eval-repair packets for the non-pass gates -------------------
EXPLAIN CT-SAVE
s" packet.certify"                 CT-NOTIN
s" packet.golden: class=not-run"   CT-IN
s" repair=run-device-gradcheck"    CT-IN
s" repro=model:FFN"                CT-IN

T-REPORT

end-package
