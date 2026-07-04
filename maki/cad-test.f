\ maki/cad-test.f - checked tests for Model CAD commands + checked MODEL: capture.
\ Capture runs the body through the planning vocabulary into the model-IR node
\ table; LOWER reports real node facts; every fail-closed probe stays green.

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
: TRY-EMPTY   ( -- )  CAP-BEGIN CAP-END ;
: TRY-NODATA  ( -- )  CAP-BEGIN OP-GELU CAP-OP ;
: TRY-ARITY   ( -- )  CAP-BEGIN 2 2 CAP-INPUT OP-LINEAR CAP-OP ;
: TRY-INPUTS  ( -- )  CAP-BEGIN CAP-CAP 1+ 0 ?do 1 1 CAP-INPUT loop ;
: TRY-SHAPE   ( -- )  s" 2y3" PARSE-SHAPE 2drop ;

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

\ known op tokens map (spot-check the fail-closed table, incl. new silu/rope) --
s" LINEAR"      OP-KIND OP-LINEAR      T=
s" GELU"        OP-KIND OP-GELU        T=
s" SILU"        OP-KIND OP-SILU        T=
s" ROPE"        OP-KIND OP-ROPE        T=
s" SOFTMAX-ROW" OP-KIND OP-SOFTMAX-ROW T=

\ ---- capture engine fail-closed paths --------------------------------------
' TRY-EMPTY   E-CAD-EMPTY  TTHROWS
' TRY-NODATA  E-CAD-ARITY  TTHROWS
' TRY-ARITY   E-CAD-ARITY  TTHROWS
' TRY-INPUTS  E-CAD-INPUTS TTHROWS
' TRY-SHAPE   E-CAD-SYNTAX TTHROWS

\ ---- capture a toy FFN by running the body through the planning vocabulary --
MODEL: FFN ( x:2x3 w1:3x4 b1:1x4 w2:4x5 b2:1x5 -- y ) LINEAR GELU LINEAR ;
MODEL-DEFINED? TTRUE
MODEL-K 3 T=

\ ---- LOWER: real node facts (op count + output shape/dtype/layout keys) -----
LOWER
dup RPT-OPS-BEFORE@   3 T=
dup RPT-OPS-AFTER@    3 T=
dup RPT-REGIONS@      3 T=
dup RPT-MATERIALIZED@ 3 T=
dup RPT-SHAPE$  s" 2x5" T$=
dup RPT-DTYPE$  s" f32" T$=
dup RPT-LAYOUT$ s" row" T$=
dup RPT-RENDER CT-SAVE  s" report.model: FFN" CT-IN
drop

\ ---- the captured IR: op sequence + operand connectivity -------------------
MIR-RENDER CT-SAVE
s" ir.nodes: 3"       CT-IN
s" node.0.op: linear" CT-IN
s" node.1.op: gelu"   CT-IN
s" node.2.op: linear" CT-IN
s" node.0.in: i0 i1 i2" CT-IN
s" node.1.in: n0"     CT-IN
s" node.2.in: n1 i3 i4" CT-IN

\ ---- FUSE: one region per node, named split reason -------------------------
FUSE
dup RPT-SPLIT-COUNT 1 T=
dup RPT-RENDER CT-SAVE  s" fusion.split.0:" CT-IN
drop

\ ---- MEMORY: bytes unknown (shapes not yet bound for a cost model) ----------
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

\ ---- EXPLAIN: repair packets for the non-pass gates ------------------------
EXPLAIN CT-SAVE
s" packet.certify"                 CT-NOTIN
s" packet.golden: class=not-run"   CT-IN
s" repair=run-device-gradcheck"    CT-IN
s" repro=model:FFN"                CT-IN

\ ---- movement ops: capture grammar, IR facts, verdicts, MEMORY rows ---------
\ concat materializes (v1); an aligned row-slice dissolves (free) - no traffic.
MODEL: MCAT ( x:2x4 b:2x4 -- y ) CONCAT SLICE:0..2 ;
MODEL-K 2 T=
MIR-RENDER CT-SAVE
s" node.0.op: concat"            CT-IN
s" node.0.shape: 4x4"           CT-IN
s" node.0.verdict: materialize" CT-IN
s" node.1.op: slice"            CT-IN
s" node.1.shape: 2x4"           CT-IN
s" node.1.verdict: free"        CT-IN
\ MEMORY flags the concat's materialization, not the free slice
MEMORY RPT-RENDER CT-SAVE
s" memory.move: node 0 concat verdict=materialize" CT-IN
s" memory.move: node 1"                            CT-NOTIN

\ transpose dissolves inside a staged region; gather is prologue-only (gathered).
MODEL: MGAT ( x:4x8 idx:3x1 -- y ) TRANSPOSE GATHER ;
MODEL-K 2 T=
MIR-RENDER CT-SAVE
s" node.0.op: transpose"        CT-IN
s" node.0.shape: 8x4"           CT-IN
s" node.0.verdict: staged"      CT-IN
s" node.1.op: gather"           CT-IN
s" node.1.shape: 3x4"           CT-IN
s" node.1.verdict: gathered"    CT-IN
\ MEMORY flags the gathered read, not the staged transpose
MEMORY RPT-RENDER CT-SAVE
s" memory.move: node 1 gather verdict=gathered" CT-IN
s" memory.move: node 0"                         CT-NOTIN

\ contiguous reshape dissolves to free (no materialization row)
MODEL: MRE ( x:4x8 -- y ) RESHAPE:8x4 ;
MODEL-K 1 T=
MIR-RENDER CT-SAVE
s" node.0.op: reshape"          CT-IN
s" node.0.shape: 8x4"           CT-IN
s" node.0.verdict: free"        CT-IN
MEMORY RPT-RENDER CT-SAVE
s" memory.move:"                CT-NOTIN

\ ---- movement fail-closed paths --------------------------------------------
: TRY-MV-RANGE   ( -- )  s" 12" PARSE-RANGE 2drop ;          \ no ".." separator
: TRY-MV-NOPARAM ( -- )  OP-RESHAPE CAP-MOVE0 ;              \ reshape needs params
: TRY-MV-RESHAPE ( -- )                                       \ element count mismatch
   TV-RESET PLAN-RESET  2 3 DT-F32 LAY-ROW TV-DESC 2 2 PLAN-RESHAPE drop ;
' TRY-MV-RANGE   E-CAD-SYNTAX TTHROWS
' TRY-MV-NOPARAM E-CAD-SYNTAX TTHROWS
' TRY-MV-RESHAPE E-TV-SHAPE   TTHROWS

T-REPORT

end-package
