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

\ ---- FUSE: real region plan + traffic estimate -----------------------------
\ FFN = LINEAR GELU LINEAR: gelu fuses as the first matmul's epilogue; the second
\ matmul splits (one contraction per region) -> 2 regions, matmul-boundary at node 2.
FUSE
dup RPT-OPS-BEFORE@   3 T=
dup RPT-OPS-AFTER@     2 T=
dup RPT-REGIONS@       2 T=
dup RPT-MATERIALIZED@  2 T=
dup RPT-SPLIT-COUNT    1 T=
dup 0 RPT-SPLIT@ s" matmul-boundary at node 2" T$=
\ traffic bytes computable (all FFN shapes bound); fusion removes the interior write
dup RPT-BYTES-KNOWN? TTRUE
dup RPT-BYTES-BEFORE@ 356 T=
dup RPT-BYTES-AFTER@  292 T=
dup RPT-RENDER CT-SAVE  s" fusion.split.0: matmul-boundary at node 2" CT-IN
drop

\ ---- MEMORY: bytes unknown (shapes not yet bound for a cost model) ----------
MEMORY
dup RPT-BYTES-KNOWN? TFALSE
dup RPT-RENDER CT-SAVE  s" memory.bytes-before: unknown" CT-IN
drop

\ ---- TILE: region 0 = linear+gelu epilogue -> gemm family, all 32 candidates --
\ candidates printed before emission; the closed-form default (smallest tile) is
\ selected; the section 7.4 cache key renders; the replay miss says "using defaults".
TILE
dup RPT-CAND-COUNT 32 T=
dup RPT-SELECT@ 0 T=
dup 0 RPT-CAND@ s" gemm-tf32-v1 bm=64 bn=64 bk=32 warps=4 stages=1" T$=
dup RPT-CACHE$ s" CDFF1E0D197DD30A|2x4|f32|row|al?|sm_87|engine-unbound|unprobed" T$=
dup RPT-RENDER CT-SAVE
s" schedule.candidate.0: gemm-tf32-v1 bm=64 bn=64 bk=32 warps=4 stages=1"    CT-IN
s" schedule.candidate.31: gemm-tf32-v1 bm=128 bn=128 bk=64 warps=8 stages=2" CT-IN
s" schedule.selected: 0"                                        CT-IN
s" cache.key: CDFF1E0D197DD30A|2x4|f32|row|al?"                 CT-IN
s" schedule: unmeasured shape class -> using defaults"          CT-IN
s" schedule: defaults (unmeasured shape class - cad-6 tunes)"   CT-IN
s" schedule: family from region 0 only (v1 limitation)"        CT-IN
drop

\ ---- CERTIFY: model-level static legality passes ---------------------------
CERTIFY dup G-CERTIFY RPT-GATE-TAG@ V-PASS T= drop

\ ---- host gates now real; profile still needs a device -----------------------
\ cad-7a: FFN (LINEAR GELU LINEAR) is reference-complete + host-executable, so GOLDEN
\ (self-consistency) and GRADCHECK both pass on host; only PROFILE needs a device.
GOLDEN
dup G-GOLDEN RPT-GATE-TAG@ V-PASS T=
dup G-GOLDEN RPT-GATE-REASON@ CT-SAVE  s" host self-consistent" CT-IN
drop
GRADCHECK dup G-GRADCHECK RPT-GATE-TAG@ V-PASS T= drop
PROFILE
dup G-PROFILE RPT-GATE-TAG@ V-NOTRUN T=
dup RPT-ROOFLINE@ RC-UNKNOWN T=
drop

\ ---- TUNE: TILE plus the honest "measurement needs device" note ------------
TUNE
dup RPT-CAND-COUNT 32 T=
dup RPT-RENDER CT-SAVE  s" tune: measurement needs device (cad-6)" CT-IN
drop

\ ---- OPTIMIZE: aggregate report, promotion refused (profile still needs device) --
OPTIMIZE
dup G-CERTIFY RPT-GATE-TAG@ V-PASS T=
dup G-GOLDEN  RPT-GATE-TAG@ V-PASS T=
dup RPT-RENDER CT-SAVE
s" gate.certify.verdict: pass"    CT-IN
s" gate.golden.verdict: pass"     CT-IN
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

\ ---- refused PROMOTE writes NO rows (no partial artifact) -------------------
\ off-device, PROMOTE-REPORT throws before PROMOTE-EVIDENCE, so the store stays empty.
STORE-RESET
' TRY-PROMOTE E-CAD-GATE TTHROWS
0 SK-KEY$ SCHED-GET nip TFALSE
0 SK-KEY$ EVID-GET  nip TFALSE

\ ---- a passing PROMOTE writes the schedules + evidence rows -----------------
\ real PROMOTE refuses off-device, so drive the writer directly with an all-pass
\ report over FFN's built region 0 to prove the row shapes without a device.
STORE-RESET
ALL-PASS PROMOTE-EVIDENCE drop
0 SK-KEY$ SCHED-GET TTRUE -1 T=                    \ selection row (ALL-PASS leaves sel unset = -1)
0 SK-KEY$ EVID-GET TTRUE
s" certify=pass|golden=pass|gradcheck=pass|profile=pass" T$=
STORE-RESET

\ ---- EXPLAIN: repair packets for the non-pass gates ------------------------
\ cad-7a: certify + golden + gradcheck all pass on host, so only PROFILE (device) emits
\ a packet.
EXPLAIN CT-SAVE
s" packet.certify"                 CT-NOTIN
s" packet.gradcheck"               CT-NOTIN
s" packet.golden"                  CT-NOTIN
s" packet.profile: class=not-run"  CT-IN
s" repair=run-device-profile"      CT-IN
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

\ ---- MEMORY: per-hot coalescing rows (cad-3) -------------------------------
\ MODEL: capture builds descriptors without buffers, so every model input reads
\ AL-UNKNOWN and reports "unaligned -> scalar" honestly; compiler-allocated output
\ writes are 16B-aligned by construction -> coalesced-v4.
MODEL: EWV ( x:2x4 -- y ) GELU RELU ;
MEMORY RPT-RENDER CT-SAVE
s" coalesce.tensor.0: i0"                             CT-IN
s" coalesce.status.0: unaligned"                      CT-IN
s" memory.align: input 0 unknown alignment -> scalar" CT-IN
s" coalesce.status.1: coalesced-v4"                   CT-IN

\ a materialized output with extent not a multiple of 4 shows a masked-tail row
MODEL: EWT ( x:2x5 -- y ) GELU RELU ;
MEMORY RPT-RENDER CT-SAVE
s" memory.tail: n1 5 mod 4 = 1" CT-IN

\ a 1xC bias into a compute op reports broadcast-register
MODEL: EWB ( x:2x4 b:1x4 -- y ) BIAS ;
MEMORY RPT-RENDER CT-SAVE
s" coalesce.status.1: broadcast" CT-IN

\ a gather's data read reports gathered (per-hot status + the movement row)
MODEL: GAT ( x:4x8 idx:3x1 -- y ) GATHER ;
MEMORY RPT-RENDER CT-SAVE
s" coalesce.status.0: gathered"                 CT-IN
s" memory.move: node 0 gather verdict=gathered" CT-IN

\ ---- TILE family selection from region 0's class mix (cad-4) ----------------
\ elementwise chain -> elementwise-v1; default block 256, max legal vec 4, grid-stride.
MODEL: EWS ( x:2x4 -- y ) GELU RELU ;
TILE
dup RPT-CAND-COUNT 18 T=
dup RPT-SELECT@ 11 T=
dup 11 RPT-CAND@ s" elementwise-v1 block=256 vec=4 grid-stride=y" T$=
dup RPT-RENDER CT-SAVE  s" schedule.candidate.17: elementwise-v1 block=512 vec=4 grid-stride=y" CT-IN
drop

\ a bare row-reduction (layernorm) -> row-reduce-v1 (not softmax); default two-pass.
MODEL: RRS ( x:4x128 -- y ) LAYERNORM ;
TILE
dup RPT-CAND-COUNT 36 T=
dup RPT-SELECT@ 0 T=
dup 0 RPT-CAND@ s" row-reduce-v1 lanes=32 rows=1 vec=1" T$=
drop

\ a softmax-row region -> softmax-row-v1; its 72-candidate space records in full,
\ proving the report holds a whole family (RPT-CAND-CAP), not the old 16-item cap.
MODEL: SMS ( x:4x128 -- y ) SOFTMAX-ROW ;
TILE
dup RPT-CAND-COUNT 72 T=
dup RPT-SELECT@ 0 T=
dup 0 RPT-CAND@ s" softmax-row-v1 lanes=32 rows=1 vec=1 online=n" T$=
dup 71 RPT-CAND@ s" softmax-row-v1 lanes=256 rows=4 vec=4 online=y" T$=
drop

T-REPORT

end-package
