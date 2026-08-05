\ maki/mha-op-test.f - the fused multi-head causal self-attention OP through the model-op
\ path (dot habu-op-mha-fused). OP-MHA is the whole GPT-2 c_attn sublayer as ONE author-
\ facing MODEL: op: the fused Q/K/V projection + combined bias, per-head batched causal
\ scaled-dot-product attention, the output projection + bias, and the residual add - the
\ existing maki/mha.f MHA-FWD/MHA-BWD threaded through the op surface as a seg-attn-style
\ fused node (unlike seg-attn it IS MODEL:-parseable). This proves the integration end to
\ end at the reference toy shape (B=2, T=4, C=6, H=2, hd=3):
\
\   (0) ATTR CODEC / CONFIG: T + H + causal round-trip through the attr cell; the workload
\       heads / seq config resolves into MHA-CFG>ATTR (the DROPOUT config-attr precedent).
\   (1) REGISTRY: OP-MHA / OP-MHA-BWD are COMPLETE (MHA-FWD / MHA-BWD bound), CLASS-MATMUL,
\       arity 5 / 6, named "mha" / "mha-bwd".
\   (2) CAPTURE: MODEL: with the MHA token is ONE arity-5 "mha" node whose output is the
\       data shape (B*T, C) - the token map (maki/cad.f) now names it.
\   (3) FORWARD PARITY: the host executor's EX-MHA equals a direct MHA-FWD reference call on
\       the same buffers, element-close (GC-CLOSE?), for a deterministic non-degenerate fill.
\   (4) GRADCHECK: the captured backward is central-difference verified END TO END w.r.t. the
\       input X and all four projection weights/biases (Wqkv covers wq|wk|wv, Wo, and the two
\       biases) - GC-RUN V-PASS over every model input slot.
\   (5) GEOMETRY (fail closed): the extents come from the BIND (input shapes + the attr H/T),
\       validated against - never sourced from - the reference's fixed SPEC: constants. A wrong
\       head count / non-causal request is the named E-MHA-GEOM reject, at the validator and
\       end to end through EX-RUN.
\   (6) BIND-SHAPES: OPTIMIZE-time re-propagation keeps the output as the data shape (B*T, C)
\       and re-checks every projection operand: Wqkv=(C,3C), bqkv=(1,3C), Wo=(C,C),
\       bo=(1,C). An inconsistent rebind is the named E-CAD-BIND-SHAPE reject (RB-MHA-CK).
\   (7) DETERMINISM: forward AND the input gradient are bit-reproducible run-twice.
\   (8) DEVICE LOWERING: an mha region is a fail-closed named reject (E-LMM-OP) - device
\       batched attention is dot habu-gb10-batched-attention-3055d565, a later capability.
\   (9) SELF-CONTAINMENT: a model with TWO stacked MHA nodes gradchecks on ALL slots of BOTH
\       nodes - EX-MHA-BWD re-runs MHA-FWD per node, so neither differentiates from the other's
\       module-private tape (the discriminating fixture).
\  (10) PRECISION: MHA is not a GEMM op (CPREC-GEMM? false), so the per-op precision grammar
\       MHA:FP16 is a fail-closed E-CPREC-OP reject; no CPREC bits live in the MHA attr.
\
\ NUMERICS: the host reference is f64 and the op path runs the SAME MHA-FWD, so forward parity
\ is exact; the comparison uses the shared GC-CLOSE? close-idiom, the FD gradcheck its own tol.

require lib/test.f
require lib/float.f
require lib/string.f
require maki/array.f
require maki/cad.f
require maki/gradcheck.f
require maki/fusion-plan.f
require maki/sched-key.f              \ FP-REGION-ID: the region handle the lowering reject reads
require maki/lower/launch.f           \ MDL-STAGE: the real device-lowering class dispatcher
require maki/prec-attr.f              \ CPREC-GEMM?: the mha precision arm (MHA:FP16 hostile grammar)
require maki/mha-op.f                 \ MHA op-surface bridge: MHA-FWD + attr codec / config / geometry / recompute

package MAKI

\ ---- toy-shape buffer sizes (B=2, T=4, C=6, H=2, hd=3) -------------------------------
MHA-BT #MC * constant MHO-XN          \ X / Y flat elems B*T*C = 48
#MC MHA-3C * constant MHO-WQKVN       \ fused projection weight C*3C = 108
#MC #MC * constant MHO-WON            \ output projection weight C*C = 36

create MHO-X    MHO-XN cells allot
create MHO-WQKV MHO-WQKVN cells allot   create MHO-BQKV MHA-3C cells allot
create MHO-WO   MHO-WON cells allot      create MHO-BO   #MC cells allot
create MHO-REFY MHO-XN cells allot                       \ direct-reference forward output
create MHO-Y1   MHO-XN cells allot   create MHO-Y2 MHO-XN cells allot   \ determinism snapshots
create MHO-DX1  MHO-XN cells allot   create MHO-DX2 MHO-XN cells allot

12 6 * constant MHO-PXN
8 24 * constant MHO-PWQN
create MHO-PX MHO-PXN cells allot
create MHO-PWQ MHO-PWQN cells allot   create MHO-PBQ 24 cells allot
create MHO-PWO 64 cells allot         create MHO-PBO 8 cells allot

\ deterministic non-degenerate fill: distinct gentle source per buffer (finite softmax, no NaN)
: MHO-FILL1 ( ptr r n n -- ) {: b:ptr n:n s:n :}
   n 0 ?do  i s + 1 + s>f  s 13 + s>f  f/  0.15 f*  b i T-SET  loop ;
: MHO-FILL ( -- )
   MHO-X    MHO-XN    3 MHO-FILL1
   MHO-WQKV MHO-WQKVN 5 MHO-FILL1
   MHO-BQKV MHA-3C    7 MHO-FILL1
   MHO-WO   MHO-WON  11 MHO-FILL1
   MHO-BO   #MC      17 MHO-FILL1 ;

: MHO-SNAP ( ptr r ptr r n -- ) {: sa:ptr da:ptr n:n :}  n 0 ?do  sa i T-GET  da i T-SET  loop ;
: MHO-CLOSE? ( ptr r ptr r n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do  a i T-GET  b i T-GET  GC-CLOSE? 0= if false unloop exit then  loop  true ;
: MHO-EXACT? ( ptr r ptr r n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do  a i T-GET  b i T-GET  f= 0= if false unloop exit then  loop  true ;

\ bind the five inputs to slots 0..4 and run the whole IR (forward node only for parity)
: MHO-BIND ( -- )
   EX-RESET
   MHO-X    0 MIR-SLOT-ID EX-BIND   MHO-WQKV 1 MIR-SLOT-ID EX-BIND
   MHO-BQKV 2 MIR-SLOT-ID EX-BIND   MHO-WO   3 MIR-SLOT-ID EX-BIND
   MHO-BO   4 MIR-SLOT-ID EX-BIND ;
: MHO-PROBE-BIND ( -- )
   0.125 MHO-PX MHO-PXN T-FILL
   0.125 MHO-PWQ MHO-PWQN T-FILL  0.125 MHO-PBQ 24 T-FILL
   0.125 MHO-PWO 64 T-FILL         0.125 MHO-PBO 8 T-FILL
   EX-RESET
   MHO-PX  0 MIR-SLOT-ID EX-BIND   MHO-PWQ 1 MIR-SLOT-ID EX-BIND
   MHO-PBQ 2 MIR-SLOT-ID EX-BIND   MHO-PWO 3 MIR-SLOT-ID EX-BIND
   MHO-PBO 4 MIR-SLOT-ID EX-BIND ;
: MHO-NO-PUB? ( -- bool )
   0 MIR-NODE-ID EX-OUT@ {: out:ptr :}
   0 MIR-NODE-ID EX-NODE-ELEMS 0 ?do
      out i T-GET -777.25 f= 0= if false unloop exit then
   loop true ;
: MHO-EX-REJECT ( -- )
   MHO-PROBE-BIND
   1 EX-PLAN-N
   -777.25 0 MIR-NODE-ID EX-OUT@ 0 MIR-NODE-ID EX-NODE-ELEMS T-FILL
   [: EX-RUN ;] catch E-MHA-GEOM T=
   MHO-NO-PUB? TTRUE ;
: MHO-OUT ( -- ptr r )  0 MIR-NODE-ID EX-OUT@ ;
: MHO-REF ( -- )  MHO-X MHO-WQKV MHO-BQKV MHO-WO MHO-BO MHO-REFY MHA-FWD ;

\ direct geometry-validator probes (extents come from the bind, checked against the constants)
: MHO-GEOM-OK   ( -- )  MHA-BT #MC #MH #MQ true MHA-GEOM-CK ;    \ the reference geometry: no throw
: MHO-GEOM-HEAD ( -- )  MHA-BT #MC #MH 1+ #MQ true MHA-GEOM-CK ; \ wrong head count -> reject
: MHO-GEOM-CAUS ( -- )  MHA-BT #MC #MH #MQ false MHA-GEOM-CK ;   \ non-causal is unimplemented -> reject
: MHO-OK-THROWS ( -- n )  [: MHO-GEOM-OK ;] catch ;             \ positive control: valid geometry does NOT throw

\ attr construction rejects values that cannot be represented without aliasing.
: MHO-PACK-TNEG ( -- )  -1 #MH true MHA-PACK drop ;
: MHO-PACK-TOOBIG ( -- )  MHA-TMASK 1+ #MH true MHA-PACK drop ;
: MHO-PACK-HNEG ( -- )  #MQ -1 true MHA-PACK drop ;
: MHO-PACK-HTOOBIG ( -- )  #MQ MHA-HMASK 1+ true MHA-PACK drop ;
: MHO-PACK-HALIAS ( -- )  #MQ #MH MHA-HMASK 1+ + true MHA-PACK drop ;

\ Production MODEL: capture failures run in-process through the audited dynamic-evaluate
\ boundary. The catch observes the exact domain throw while the real parser, composer,
\ plan store, and CAP-FINISH path run. Configuration is restored even after a rejection.
variable MHO-EVAL-A
variable MHO-EVAL-U
: MHO-EVAL-GO ( -- )  MHO-EVAL-A @ MHO-EVAL-U @ INCLUDE-EVALUATE ;
: MHO-EVAL-CATCH ( ptr u8 n -- n ) {: src:ptr srcu:n :}
   MHA-HEADS@ {: h:n :}
   MHA-SEQ@ {: t:n :}
   src MHO-EVAL-A !  srcu MHO-EVAL-U !
   [: MHO-EVAL-GO ;] catch {: rc:n :}
   t MHA-SEQ!  h MHA-HEADS!
   rc ;
: MHO-CAP-REJECT ( ptr u8 n n -- ) {: src:ptr srcu:n expected:n :}
   src srcu MHO-EVAL-CATCH expected T=
   MODEL-DEFINED? TFALSE
   MODEL-K 0 T= ;

: MHO-CAP-WQR$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-WQR ( x:8x6 wqkv:5x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-WQC$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-WQC ( x:8x6 wqkv:6x17 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-BQR$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-BQR ( x:8x6 wqkv:6x18 bqkv:2x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-BQC$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-BQC ( x:8x6 wqkv:6x18 bqkv:1x17 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-WOR$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-WOR ( x:8x6 wqkv:6x18 bqkv:1x18 wo:5x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-WOC$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-WOC ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x5 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-BOR$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-BOR ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:2x6 -- y ) MHA ;" ;
: MHO-CAP-BOC$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-BOC ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x5 -- y ) MHA ;" ;
: MHO-CAP-XBT$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-XBT ( x:7x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-XHD$ ( -- ptr u8 n )
   s" MODEL: MHO-CAP-XHD ( x:8x5 wqkv:5x15 bqkv:1x15 wo:5x5 bo:1x5 -- y ) MHA ;" ;
: MHO-CAP-TNEG$ ( -- ptr u8 n )
   s" -1 MHA-SEQ! MODEL: MHO-CAP-TNEG ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-TOOBIG$ ( -- ptr u8 n )
   s" MHA-TMASK 1+ MHA-SEQ! MODEL: MHO-CAP-TOOBIG ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-HNEG$ ( -- ptr u8 n )
   s" -1 MHA-HEADS! MODEL: MHO-CAP-HNEG ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-HTOOBIG$ ( -- ptr u8 n )
   s" MHA-HMASK 1+ MHA-HEADS! MODEL: MHO-CAP-HTOOBIG ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;
: MHO-CAP-HALIAS$ ( -- ptr u8 n )
   s" #MH MHA-HMASK 1+ + MHA-HEADS! MODEL: MHO-CAP-HALIAS ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;" ;

\ Device lowering must reject through the class dispatcher used by whole-model runs.
: MHO-TRY-MDL-FWD ( -- )  0 FP-REGION-ID MDL-STAGE ;
: MHO-TRY-MDL-BWD ( -- )  1 MIR-NODE-ID FP-RID@ MDL-STAGE ;

\ hostile precision grammar: MHA is not a GEMM op (CPREC-GEMM? false), so a per-op precision
\ tag is illegal - the production translator throws E-CPREC-OP (prec-grammar-test.f idiom).
: MHO-TRY-FP16 ( -- )  CAP-BEGIN  s" MHA:FP16" EMIT-OP-TOKEN ;

T-RESET

\ ============================ (0) attr codec + config =========================
4 2 true MHA-PACK  MHA-T@ 4 T=                              \ T round-trips
4 2 true MHA-PACK  MHA-H@ 2 T=                              \ H round-trips
4 2 true MHA-PACK  MHA-CAUSAL@ TTRUE                        \ causal flag round-trips
9 5 false MHA-PACK MHA-T@ 9 T=                              \ distinct T
9 5 false MHA-PACK MHA-H@ 5 T=                              \ distinct H
9 5 false MHA-PACK MHA-CAUSAL@ TFALSE                       \ non-causal flag round-trips
2 MHA-HEADS!  4 MHA-SEQ!
MHA-HEADS@ 2 T=   MHA-SEQ@ 4 T=                             \ config setters/getters
MHA-CFG>ATTR MHA-H@ 2 T=   MHA-CFG>ATTR MHA-T@ 4 T=         \ config resolves into the attr
MHA-CFG>ATTR MHA-CAUSAL@ TTRUE                              \ v1 config is causal-only
' MHO-PACK-TNEG E-MHA-GEOM TTHROWS
' MHO-PACK-TOOBIG E-MHA-GEOM TTHROWS
' MHO-PACK-HNEG E-MHA-GEOM TTHROWS
' MHO-PACK-HTOOBIG E-MHA-GEOM TTHROWS
' MHO-PACK-HALIAS E-MHA-GEOM TTHROWS

\ combined-gradient buffer layout: cumulative CELL boundaries dX|dWqkv|dbqkv|dWo|dbo
MHA-BT #MC 0 MHA-BWD-OFF 0 T=                               \ dX starts at 0
MHA-BT #MC 1 MHA-BWD-OFF MHO-XN T=                          \ dWqkv after dX (B*T*C)
MHA-BT #MC 5 MHA-BWD-OFF  MHO-XN MHO-WQKVN + MHA-3C + MHO-WON + #MC +  T=   \ TOTAL

\ ============================ (1) registry ===================================
MAKI-OPKIND:MHA     OPR-COMPLETE? TTRUE
MAKI-OPKIND:MHA-BWD OPR-COMPLETE? TTRUE
MAKI-OPKIND:MHA     OPR-ARITY 5 T=
MAKI-OPKIND:MHA-BWD OPR-ARITY 6 T=                          \ self-contained: 5 forward inputs + dY
MAKI-OPKIND:MHA     OPR-NAME s" mha"     STR= TTRUE
MAKI-OPKIND:MHA-BWD OPR-NAME s" mha-bwd" STR= TTRUE
MAKI-OPKIND:MHA     OPR-CLASS CLASS-MATMUL T=

\ ============================ (2) capture ====================================
MODEL: MHO-M ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
MODEL-K 1 T=                                               \ one op node
0 MIR-NODE-ID MIR-OP@ OPR-NAME s" mha" STR= TTRUE
0 MIR-NODE-ID MIR-IN-COUNT@ 5 T=                           \ arity 5 (X, Wqkv, bqkv, Wo, bo)
0 MIR-NODE-ID MIR-ROWS@ ROWS-RAW MHA-BT T=                 \ output rows = B*T
0 MIR-NODE-ID MIR-COLS@ COLS-RAW #MC   T=                  \ output cols = C
MHO-CAP-WQR$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-WQC$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-BQR$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-BQC$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-WOR$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-WOC$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-BOR$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-BOC$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-XBT$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-XHD$ E-CAD-PARAM-SHAPE MHO-CAP-REJECT
MHO-CAP-TNEG$ E-MHA-GEOM MHO-CAP-REJECT
MHO-CAP-TOOBIG$ E-MHA-GEOM MHO-CAP-REJECT
MHO-CAP-HNEG$ E-MHA-GEOM MHO-CAP-REJECT
MHO-CAP-HTOOBIG$ E-MHA-GEOM MHO-CAP-REJECT
MHO-CAP-HALIAS$ E-MHA-GEOM MHO-CAP-REJECT
MHA-HEADS@ 2 T=
MHA-SEQ@ 4 T=

\ C=2,500,000,000 keeps 3C representable while C*C exceeds the tensor element-count domain.
\ BW-BUILD must reject through SHAPE-ELEMS before any gradient row boundary is published.
MODEL: MHO-BW-OVER ( x:8x2500000000 wqkv:2500000000x7500000000 bqkv:1x7500000000 wo:2500000000x2500000000 bo:1x2500000000 -- y ) MHA ;
' BW-BUILD E-MK-DIM TTHROWS

\ ============================ (3) forward parity =============================
MHO-FILL
MODEL: MHO-FWD ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
MHO-BIND EX-RUN
MHO-OUT MHO-Y1 MHO-XN MHO-SNAP                             \ the op-path forward
MHO-REF                                                    \ the direct MHA-FWD reference
MHO-Y1 MHO-REFY MHO-XN MHO-CLOSE? TTRUE                    \ op path == reference, element-close

\ ============================ (4) gradcheck =================================
MODEL: MHO-GC ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
GC-RUN V-PASS T=                                           \ dX and every weight/bias grad match central FD
GC-RE$ s" 5 input(s) gradchecked" CONTAINS? TTRUE

\ ============================ (5) geometry fail-closed =======================
MHO-OK-THROWS 0 T=                                         \ the reference geometry does NOT throw
' MHO-GEOM-HEAD E-MHA-GEOM TTHROWS                         \ wrong head count is rejected
' MHO-GEOM-CAUS E-MHA-GEOM TTHROWS                         \ non-causal is rejected (unimplemented)
\ Structurally valid non-oracle models reach EX-RUN's fixed-kernel geometry guard. The
\ pre-seeded output stays unchanged, proving the failed execution publishes no tensor data.
MODEL: MHO-BADB ( x:12x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
MHO-EX-REJECT                                                \ B=3, not the oracle B=2
2 MHA-SEQ!
MODEL: MHO-BADT ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
4 MHA-SEQ!
MHO-EX-REJECT                                                \ T=2, not the oracle T=4
MODEL: MHO-BADC ( x:8x8 wqkv:8x24 bqkv:1x24 wo:8x8 bo:1x8 -- y ) MHA ;
MHO-EX-REJECT                                                \ C=8 and head width 4, not C=6 / width 3
3 MHA-HEADS!                                               \ the attr now carries H=3
MODEL: MHO-BADH ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
2 MHA-HEADS!                                               \ restore the reference config
MHO-EX-REJECT                                                \ H=3, not the oracle H=2

\ ============================ (6) bind-shapes reprop ========================
\ Wqkv/bqkv cols left unbound (0) so BIND-SHAPES drives RB-MHA-CK: a consistent rebind keeps
\ the output at (B*T, C) and passes. Isolated rejects below pin every row and column relation.
MODEL: MHO-RB ( x:8x6 wqkv:6x0 bqkv:1x0 wo:6x6 bo:1x6 -- y ) MHA ;
: MHO-BIND-OK ( -- )
   BS-RESET  8 6 BS-PUSH  6 18 BS-PUSH  1 18 BS-PUSH
   6 6 BS-PUSH  1 6 BS-PUSH  BS-BIND ;
: MHO-BIND-WQR-BAD ( -- )
   BS-RESET  8 6 BS-PUSH  5 18 BS-PUSH  1 18 BS-PUSH
   6 6 BS-PUSH  1 6 BS-PUSH  BS-BIND ;
: MHO-BIND-WQC-BAD ( -- )
   BS-RESET  8 6 BS-PUSH  6 17 BS-PUSH  1 18 BS-PUSH
   6 6 BS-PUSH  1 6 BS-PUSH  BS-BIND ;
: MHO-BIND-BQR-BAD ( -- )
   BS-RESET  8 6 BS-PUSH  6 18 BS-PUSH  2 18 BS-PUSH
   6 6 BS-PUSH  1 6 BS-PUSH  BS-BIND ;
: MHO-BIND-BQC-BAD ( -- )
   BS-RESET  8 6 BS-PUSH  6 18 BS-PUSH  1 17 BS-PUSH
   6 6 BS-PUSH  1 6 BS-PUSH  BS-BIND ;
: MHO-BIND-WOR-BAD ( -- )
   BS-RESET  8 6 BS-PUSH  6 18 BS-PUSH  1 18 BS-PUSH
   5 6 BS-PUSH  1 6 BS-PUSH  BS-BIND ;
: MHO-BIND-WOC-BAD ( -- )
   BS-RESET  8 6 BS-PUSH  6 18 BS-PUSH  1 18 BS-PUSH
   6 5 BS-PUSH  1 6 BS-PUSH  BS-BIND ;
: MHO-BIND-BOR-BAD ( -- )
   BS-RESET  8 6 BS-PUSH  6 18 BS-PUSH  1 18 BS-PUSH
   6 6 BS-PUSH  2 6 BS-PUSH  BS-BIND ;
: MHO-BIND-BOC-BAD ( -- )
   BS-RESET  8 6 BS-PUSH  6 18 BS-PUSH  1 18 BS-PUSH
   6 6 BS-PUSH  1 5 BS-PUSH  BS-BIND ;
: MHO-BIND-XBT-BAD ( -- )
   BS-RESET  7 6 BS-PUSH  6 18 BS-PUSH  1 18 BS-PUSH
   6 6 BS-PUSH  1 6 BS-PUSH  BS-BIND ;
: MHO-BIND-XHD-BAD ( -- )
   BS-RESET  8 5 BS-PUSH  5 15 BS-PUSH  1 15 BS-PUSH
   5 5 BS-PUSH  1 5 BS-PUSH  BS-BIND ;
MHO-BIND-OK                                                \ consistent rebind: Wqkv=(C,3C)
0 MIR-NODE-ID MIR-ROWS@ ROWS-RAW MHA-BT T=                 \ output re-propagates to the DATA shape (B*T, C)...
0 MIR-NODE-ID MIR-COLS@ COLS-RAW #MC   T=                  \ ...not the fused 3C contraction width
MODEL: MHO-RBWQR ( x:8x6 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-WQR-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBWQC ( x:8x6 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-WQC-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBBQR ( x:8x6 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-BQR-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBBQC ( x:8x6 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-BQC-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBWOR ( x:8x6 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-WOR-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBWOC ( x:8x6 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-WOC-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBBOR ( x:8x6 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-BOR-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBBOC ( x:8x6 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-BOC-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBXBT ( x:0x0 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-XBT-BAD E-CAD-BIND-SHAPE TTHROWS
MODEL: MHO-RBXHD ( x:0x0 wqkv:0x0 bqkv:0x0 wo:0x0 bo:0x0 -- y ) MHA ;
' MHO-BIND-XHD-BAD E-CAD-BIND-SHAPE TTHROWS

\ ============================ (7) determinism: run-twice locks ================
MODEL: MHO-DET ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
MHO-FILL
MHO-BIND EX-RUN  MHO-OUT MHO-Y1 MHO-XN MHO-SNAP
MHO-BIND EX-RUN  MHO-OUT MHO-Y2 MHO-XN MHO-SNAP
MHO-Y1 MHO-Y2 MHO-XN MHO-EXACT? TTRUE                      \ forward locked run-twice
\ the input gradient is bit-reproducible too (backward run-twice)
BW-BUILD
MHO-BIND  MHO-X BW-SEED-SLOT@ EX-BIND  EX-RUN
0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ MHO-DX1 MHO-XN MHO-SNAP
MHO-BIND  MHO-X BW-SEED-SLOT@ EX-BIND  EX-RUN
0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ MHO-DX2 MHO-XN MHO-SNAP
MHO-DX1 MHO-DX2 MHO-XN MHO-EXACT? TTRUE                    \ dX locked run-twice

\ ============================ (8) device lowering reject ====================
MODEL: MHO-LOW ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
FP-BUILD
' MHO-TRY-MDL-FWD E-LMM-OP TTHROWS                         \ forward fails through the real dispatcher
MODEL: MHO-LOW-BWD ( x:8x6 wqkv:6x18 bqkv:1x18 wo:6x6 bo:1x6 -- y ) MHA ;
BW-BUILD
1 MIR-NODE-ID MIR-OP@ MAKI-OPKIND:MHA-BWD MAKI-OPKIND:EQ TTRUE
FP-BUILD
' MHO-TRY-MDL-BWD E-LMM-OP TTHROWS                         \ synthesized backward fails there too

\ ============================ (9) two-MHA-node self-containment ==============
\ THE discriminating fixture: two stacked MHA nodes (the second reads the first's output as
\ its X). MHA-BWD reads a module-private tape; without self-containment the first node's
\ backward would differentiate from the SECOND node's tape (silently wrong grads). Because
\ EX-MHA-BWD re-runs MHA-FWD from each node's OWN five inputs, gradcheck passes on ALL slots
\ of BOTH nodes (9 inputs). A stale-tape backward reds this.
MODEL: MHO-TWO ( x:8x6 wqkv1:6x18 bqkv1:1x18 wo1:6x6 bo1:1x6 wqkv2:6x18 bqkv2:1x18 wo2:6x6 bo2:1x6 -- y ) MHA MHA ;
MODEL-K 2 T=                                               \ two op nodes
GC-RUN V-PASS T=                                           \ every slot of both nodes gradchecks
GC-RE$ s" 9 input(s) gradchecked" CONTAINS? TTRUE

\ ============================ (10) precision grammar (fail closed) ===========
MAKI-OPKIND:MHA     CPREC-GEMM? TFALSE                     \ MHA is not a precision-taggable GEMM op
MAKI-OPKIND:MHA-BWD CPREC-GEMM? TFALSE
' MHO-TRY-FP16 E-CPREC-OP TTHROWS                          \ MHA:FP16 is illegal grammar (no CPREC bits in the attr)

T-REPORT

;package
