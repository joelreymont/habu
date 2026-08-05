\ maki/dropout-op-test.f - the dropout op (train/eval) + its VJP through the model-op path
\ (dot habu-dropout-op-train). Inverted dropout: TRAIN zeroes each element with prob p and
\ scales survivors by 1/(1-p); EVAL is the identity. Mode/p are OP ATTRIBUTES (not arity),
\ decoded at the boundary (DO-EVAL? / DO-FORM), the affine-LayerNorm form idiom. This proves
\ the op integration end to end:
\
\   (1) REGISTRY: OP-DROPOUT / OP-DROPOUT-BWD are complete (host reference bound), arity 1 / 2.
\   (2) ATTR-BASED IDENTITY: a captured node's mode is READ from the stored attr, never the
\       input count - a train and an eval dropout are both arity-1 "dropout" nodes.
\   (3) FORWARD GOLDEN: train masks-and-scales (each y is 0 or x*1/(1-p) exactly); eval is
\       bit-identical to x; train with p=0 is bit-identical to x (no eps perturbation).
\   (4) DETERMINISM: a seeded run is bit-reproducible (EX-RUN twice -> identical y).
\   (5) GRADCHECK: the captured backward is FD-verified END TO END (GC-RUN, mask held fixed
\       for free), and the VJP's mask matches the forward's - dropped grads are EXACTLY zero.
\   (6) STATISTICAL GOLDEN: over a large sample the keep rate matches 1-p and E[y] ~= E[x].
\   (7) DOMAIN GUARDS: p<0, p>=1, non-finite p are the named E-DO-P reject (red-first).
\   (8) DEVICE LOWERING: a dropout region is a fail-closed named reject (E-LEW-OP) - device
\       dropout is a later capability, no silent wrong lowering.
\   (9) INTEGRATION: a small model with dropout BETWEEN layers is deterministic run-twice in
\       train (fwd AND grads locked) and, switched to eval, is identical to the dropout-free
\       model - the train/eval switch proven end to end.

require lib/test.f
require lib/float.f
require lib/string.f
require maki/array.f
require maki/cad.f
require maki/gradcheck.f
require maki/fusion-plan.f
require maki/sched-key.f              \ FP-REGION-ID: the region handle the lowering reject reads
require maki/lower/ew.f               \ LEW-ANALYZE / E-LEW-OP: the device-lowering reject arm

package MAKI

\ ---- shared fixtures ---------------------------------------------------------
32 constant DRC          \ 4x8 elems
create DR-X   DRC cells allot
create DR-Y1  DRC cells allot   create DR-Y2 DRC cells allot
create DR-SEED-CT DRC cells allot                     \ seed-cotangent buffer (all ones)

: DR-FILL1 ( ptr r n -- ) {: p:ptr n:n :}  n 0 ?do  1.0 p i T-SET  loop ;   \ all ones
: DR-SNAP ( ptr r ptr r n -- ) {: sa:ptr da:ptr n:n :}  n 0 ?do  sa i T-GET  da i T-SET  loop ;

: DR-ALL-EQ? ( ptr r ptr r n -- bool ) {: a:ptr b:ptr n:n :}   \ every element bit-equal (f=)
   n 0 ?do  a i T-GET  b i T-GET  f= 0= if  false unloop exit  then  loop  true ;

\ each y is EITHER 0 OR x*scale exactly (the inverted-dropout per-element golden)
: DR-MASK-OK? ( ptr r ptr r n r -- bool ) {: yb:ptr xb:ptr n:n sc:r :}
   n 0 ?do
      yb i T-GET  dup 0.0 f=  swap  xb i T-GET sc f*  f=  or 0= if  false unloop exit  then
   loop  true ;

: DR-COUNT-KEPT ( ptr r n -- n ) {: yb:ptr n:n :}   \ non-zero (kept) elements
   0  n 0 ?do  yb i T-GET 0.0 f= 0= if 1+ then  loop ;
: DR-MEAN ( ptr r n -- r ) {: yb:ptr n:n :}
   0.0  n 0 ?do  yb i T-GET f+  loop  n s>f f/ ;

: DR-OUT ( -- ptr r )  0 MIR-NODE-ID EX-OUT@ ;
: DR-BIND-RUN ( -- )  EX-RESET  DR-X 0 MIR-SLOT-ID EX-BIND  EX-RUN ;

\ direct (catchable) probes of the p domain guard (DO-P>FIX, the composer's codec)
: DR-INF ( -- r )  1.0 0.0 f/ ;                       \ +Inf (non-finite)
: DR-P-NEG ( -- )  0.0 0.1 f- DO-P>FIX drop ;
: DR-P-ONE ( -- )   1.0 DO-P>FIX drop ;
: DR-P-BIG ( -- )   1.5 DO-P>FIX drop ;
: DR-P-INF ( -- )  DR-INF DO-P>FIX drop ;
: DR-P-OK-THROWS ( -- n )  [: 0.5 DO-P>FIX drop ;] catch ;   \ positive control: a legal p must NOT throw
: DR-CFG-BAD ( -- )  1.5 DROPOUT-P! DO-CFG>ATTR drop ;        \ the composer path guards too

\ device-lowering reject: analyze a captured dropout region as an elementwise kernel
: DR-TRY-LEW ( -- )  0 FP-REGION-ID LEW-ANALYZE ;

\ statistical golden: 64x64 sample, keep rate + E[y]
4096 constant DR-N2
create DR-X2 DR-N2 cells allot
: DR-BIND-RUN2 ( -- )  DR-X2 DR-N2 DR-FILL1  EX-RESET  DR-X2 0 MIR-SLOT-ID EX-BIND  EX-RUN ;

T-RESET
DR-X DRC DR-FILL1
DR-SEED-CT DRC DR-FILL1

\ ============================ (1) registry ====================================
MAKI-OPKIND:DROPOUT     OPR-COMPLETE? TTRUE
MAKI-OPKIND:DROPOUT-BWD OPR-COMPLETE? TTRUE
MAKI-OPKIND:DROPOUT     OPR-ARITY 1 T=                       \ forward: x only (mode/p ride the attr)
MAKI-OPKIND:DROPOUT-BWD OPR-ARITY 2 T=                       \ VJP: dy + the forward node (its seed index)
MAKI-OPKIND:DROPOUT     OPR-NAME s" dropout" STR= TTRUE

\ ============================ (2) attribute-based identity =====================
MAKI-DOMODE:TRAIN DROPOUT-MODE!  0.5 DROPOUT-P!  12345 DROPOUT-SEED!
MODEL: DR-MT ( x:4x8 -- y ) DROPOUT ;
MODEL-K 1 T=
0 MIR-NODE-ID MIR-OP@ OPR-NAME s" dropout" STR= TTRUE
0 MIR-NODE-ID MIR-IN-COUNT@ 1 T=                             \ arity 1 for BOTH modes (mode is not arity)
0 MIR-NODE-ID DO-EVAL? TFALSE                                \ train node -> mode read as train
0 MIR-NODE-ID DO-FORM MAKI-DOMODE:TRAIN MAKI-DOMODE:EQ TTRUE
MAKI-DOMODE:EVAL DROPOUT-MODE!
MODEL: DR-ME ( x:4x8 -- y ) DROPOUT ;
0 MIR-NODE-ID MIR-IN-COUNT@ 1 T=                             \ still arity 1
0 MIR-NODE-ID DO-EVAL? TTRUE                                 \ eval node -> mode read as eval
0 MIR-NODE-ID DO-FORM MAKI-DOMODE:EVAL MAKI-DOMODE:EQ TTRUE

\ ============================ (3) forward golden ==============================
\ train p=0.5 -> scale 2.0; each y is 0 (dropped) or 2*x (kept), and both classes occur.
MAKI-DOMODE:TRAIN DROPOUT-MODE!  0.5 DROPOUT-P!  12345 DROPOUT-SEED!
MODEL: DR-FWD ( x:4x8 -- y ) DROPOUT ;
DR-BIND-RUN
DR-OUT DR-X DRC 2.0 DR-MASK-OK? TTRUE                        \ every element is 0 or x*2 exactly
DR-OUT DRC DR-COUNT-KEPT 0 > TTRUE                           \ some kept
DR-OUT DRC DR-COUNT-KEPT DRC < TTRUE                         \ some dropped

\ eval p=0.5 -> identity (bit-equal to x, regardless of p)
MAKI-DOMODE:EVAL DROPOUT-MODE!  0.5 DROPOUT-P!
MODEL: DR-EV ( x:4x8 -- y ) DROPOUT ;
DR-BIND-RUN
DR-OUT DR-X DRC DR-ALL-EQ? TTRUE

\ train p=0 -> identity (mask all-keep, scale 1.0), no eps perturbation
MAKI-DOMODE:TRAIN DROPOUT-MODE!  0.0 DROPOUT-P!
MODEL: DR-P0 ( x:4x8 -- y ) DROPOUT ;
DR-BIND-RUN
DR-OUT DR-X DRC DR-ALL-EQ? TTRUE

\ ============================ (4) determinism: run-twice locks ================
MAKI-DOMODE:TRAIN DROPOUT-MODE!  0.5 DROPOUT-P!  777 DROPOUT-SEED!
MODEL: DR-DET ( x:4x8 -- y ) DROPOUT ;
DR-BIND-RUN  DR-OUT DR-Y1 DRC DR-SNAP
DR-BIND-RUN  DR-OUT DR-Y2 DRC DR-SNAP
DR-Y1 DR-Y2 DRC DR-ALL-EQ? TTRUE                             \ seeded run is bit-reproducible

\ ============================ (5) gradcheck + dropped-grad-is-zero ============
MAKI-DOMODE:TRAIN DROPOUT-MODE!  0.5 DROPOUT-P!  9001 DROPOUT-SEED!
MODEL: DR-GC ( x:4x8 -- y ) DROPOUT ;
GC-RUN V-PASS T=                                             \ dx matches central FD (mask held fixed)
GC-RE$ s" host: 1 input(s) gradchecked" CONTAINS? TTRUE

\ x=1, ct=1 -> forward y = mask*scale AND grad = mask*scale, so the VJP mask must equal the
\ forward mask bit-for-bit (dropped -> exactly 0, kept -> exactly scale). Build the backward,
\ run fwd+bwd, and compare the input gradient buffer to the forward output element-wise.
MODEL: DR-GZ ( x:4x8 -- y ) DROPOUT ;
BW-BUILD
EX-RESET
DR-X 0 MIR-SLOT-ID EX-BIND
DR-SEED-CT BW-SEED-SLOT@ EX-BIND
EX-RUN
0 MIR-NODE-ID EX-OUT@  0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@  DRC DR-ALL-EQ? TTRUE
0 MIR-NODE-ID EX-OUT@ DRC DR-COUNT-KEPT 0 > TTRUE            \ (non-trivial: some elements survive)
0 MIR-NODE-ID EX-OUT@ DRC DR-COUNT-KEPT DRC < TTRUE          \ (non-trivial: some dropped -> zero grad)

\ ============================ (6) statistical golden =========================
\ 4096 samples at a fixed seed, p=0.25: keep rate ~= 0.75 and E[y] ~= E[x]=1 (the 1/(1-p) scaling).
MAKI-DOMODE:TRAIN DROPOUT-MODE!  0.25 DROPOUT-P!  424242 DROPOUT-SEED!
MODEL: DR-STAT ( x:64x64 -- y ) DROPOUT ;
DR-BIND-RUN2
0 MIR-NODE-ID EX-OUT@ DR-N2 DR-COUNT-KEPT 3019 T=            \ exact kept count for THIS seed (deterministic golden)
0 MIR-NODE-ID EX-OUT@ DR-N2 DR-COUNT-KEPT s>f DR-N2 s>f f/  0.75 f- fabs 0.02 f< TTRUE  \ keep rate ~= 1-p
0 MIR-NODE-ID EX-OUT@ DR-N2 DR-MEAN 1.0 f- fabs 0.02 f< TTRUE \ E[y] ~= E[x] (survivor scaling proven)

\ ============================ (7) domain guards (red-first) ==================
' DR-P-NEG E-DO-P TTHROWS
' DR-P-ONE E-DO-P TTHROWS
' DR-P-BIG E-DO-P TTHROWS
' DR-P-INF E-DO-P TTHROWS
' DR-CFG-BAD E-DO-P TTHROWS                                  \ the composer's codec guards too
DR-P-OK-THROWS 0 T=                                          \ a legal p does NOT throw

\ ============================ (8) device lowering reject =====================
MAKI-DOMODE:TRAIN DROPOUT-MODE!  0.5 DROPOUT-P!
MODEL: DR-LOW ( x:4x8 -- y ) DROPOUT ;
FP-BUILD
' DR-TRY-LEW E-LEW-OP TTHROWS                                \ no silent wrong device lowering

\ ============================ (9) integration: dropout between layers ========
\ train mode: the whole fwd+bwd computation is bit-reproducible run-twice.
MAKI-DOMODE:TRAIN DROPOUT-MODE!  0.5 DROPOUT-P!  20260720 DROPOUT-SEED!
MODEL: DR-NET ( x:4x8 -- y ) GELU DROPOUT GELU ;
BW-BUILD
EX-RESET
DR-X 0 MIR-SLOT-ID EX-BIND
DR-SEED-CT BW-SEED-SLOT@ EX-BIND
EX-RUN
BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ DR-Y1 DRC DR-SNAP          \ forward output (run 1)
0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ DR-Y2 DRC DR-SNAP   \ input grad (run 1)
EX-RUN
BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ DR-Y1 DRC DR-ALL-EQ? TTRUE  \ forward locked run-twice
0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ DR-Y2 DRC DR-ALL-EQ? TTRUE   \ grads locked run-twice

\ train-mode multi-layer model gradchecks (correct gradients flow through the dropout = trainable)
MODEL: DR-NETGC ( x:4x8 -- y ) GELU DROPOUT GELU ;
GC-RUN V-PASS T=

\ eval switch: with dropout disabled the model is IDENTICAL to the dropout-free reference.
MODEL: DR-REF ( x:4x8 -- y ) GELU GELU ;
DR-BIND-RUN  1 MIR-NODE-ID EX-OUT@ DR-Y1 DRC DR-SNAP        \ reference output (last node)
MAKI-DOMODE:EVAL DROPOUT-MODE!
MODEL: DR-NETE ( x:4x8 -- y ) GELU DROPOUT GELU ;
DR-BIND-RUN  2 MIR-NODE-ID EX-OUT@ DR-Y1 DRC DR-ALL-EQ? TTRUE   \ eval dropout == no dropout

T-REPORT

;package
