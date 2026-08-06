\ maki/swiglu-op-test.f - the SwiGLU op (silu(gate)*up) + its VJP through the model-op
\ path (dot habu-infer-swiglu-op). OP-SWIGLU is a FUSED arity-2 elementwise op: both
\ operands are same-shape DATA (gate, up), no attrs. This proves the op integration end
\ to end:
\
\   (1) REGISTRY: OP-SWIGLU is COMPLETE (SWIGLU-F bound), CLASS-EW, arity 2.
\   (2) CAPTURE: MODEL: capture of the binary op is one arity-2 "swiglu" node.
\   (3) FORWARD GOLDEN: the host executor's EX-EW2 gives y[i] = silu(gate[i])*up[i]
\       BIT-EXACTLY vs the maki/swiglu.f SWIGLU-F reference (same f64 op), over a
\       fixture that spans negative/zero/positive gates (sigmoid saturates, no NaN).
\   (4) SHAPE GUARD: EW-SHAPE-CHECK under the SWIGLU class rejects a mismatched-shape
\       up operand (E-CAD-PARAM-SHAPE, like OP-MUL); an equal shape passes.
\   (5) VJP EXACT: the captured backward decomposes into OP-SILU/OP-MUL/OP-SILU-BWD
\       (no dedicated *-BWD op); its d_gate and d_up buffers equal the closed forms
\       SWIGLU-DGATE / SWIGLU-DUP BIT-EXACTLY (same f64 ops), for a non-trivial (mixed
\       sign) cotangent.
\   (6) GRADCHECK: the same backward is central-difference verified END TO END w.r.t.
\       BOTH inputs (GC-RUN), the bcast-mul precedent.
\   (7) DETERMINISM: forward AND grads are bit-reproducible run-twice (run-twice locked).
\   (8) DEVICE LOWERING: a swiglu region is a fail-closed named reject (E-LEW-OP) - device
\       SwiGLU in the model-IR path is a later capability; the standalone device kernel is
\       tools/ptx/swiglu-cg.f (proven on the GB10 by tools/ptx/swiglu-device-test.f).
\   (9) INTEGRATION: SwiGLU composed with GELU gradchecks (correct gradients flow through).
\
\ NUMERICS: sigmoid is the SHARED maki/fmath.f SIGMOID-F (= 1/(1+exp(-x)), the same stable
\ form OP-SILU/OP-GELU use): it saturates to 0/1 at the extremes with no NaN over the
\ committed fixture range |gate| <= 4. Host reference is f64, so the forward and VJP goldens
\ are BIT-EXACT (f=); the FD gradcheck carries its own tolerance.

require lib/test.f
require lib/float.f
require lib/string.f
require maki/array.f
require maki/cad.f
require maki/gradcheck.f
require maki/fusion-plan.f
require maki/sched-key.f              \ FP-REGION-ID: the region handle the lowering reject reads
require maki/lower/ew.f               \ LEW-ANALYZE / E-LEW-OP: the device-lowering reject arm
require maki/swiglu.f                 \ SWIGLU-F / SWIGLU-DGATE / SWIGLU-DUP: the goldens

package MAKI

8 constant SWC          \ 2x4 elems
create SW-GATE SWC cells allot   create SW-UP  SWC cells allot
create SW-CT   SWC cells allot   create SW-Y1  SWC cells allot   create SW-Y2 SWC cells allot

\ fixture: gate spans negative / zero / positive (sigmoid saturation, no NaN); up mixed sign.
: SW-FILL ( -- )
   -2.0 SW-GATE 0 T-SET  -0.5 SW-GATE 1 T-SET   0.0 SW-GATE 2 T-SET   0.5 SW-GATE 3 T-SET
    2.0 SW-GATE 4 T-SET  -3.0 SW-GATE 5 T-SET   1.5 SW-GATE 6 T-SET   4.0 SW-GATE 7 T-SET
    1.0 SW-UP   0 T-SET  -1.0 SW-UP   1 T-SET   2.0 SW-UP   2 T-SET   0.5 SW-UP   3 T-SET
   -0.5 SW-UP   4 T-SET   3.0 SW-UP   5 T-SET  -2.0 SW-UP   6 T-SET   1.0 SW-UP   7 T-SET ;
\ cotangent: non-trivial (mixed sign, not all ones), so the VJP golden is not degenerate.
: SW-CT-FILL ( -- )
   0.7 SW-CT 0 T-SET  -1.3 SW-CT 1 T-SET  0.2 SW-CT 2 T-SET  2.0 SW-CT 3 T-SET
  -0.4 SW-CT 4 T-SET   1.1 SW-CT 5 T-SET  0.9 SW-CT 6 T-SET -0.6 SW-CT 7 T-SET ;

: SW-SNAP ( ptr r ptr r n -- ) {: sa:ptr da:ptr n:n :}  n 0 ?do  sa i T-GET  da i T-SET  loop ;
: SW-ALL-EQ? ( ptr r ptr r n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do  a i T-GET  b i T-GET  f= 0= if  false unloop exit  then  loop  true ;

: SW-OUT ( -- ptr r )  0 MIR-NODE-ID EX-OUT@ ;
: SW-BIND-RUN ( -- )
   SW-FILL EX-RESET  SW-GATE 0 MIR-SLOT-ID EX-BIND  SW-UP 1 MIR-SLOT-ID EX-BIND  EX-RUN ;

\ forward golden: every y[i] is silu(gate[i])*up[i] bit-exactly vs the SWIGLU-F reference.
: SW-FWD-OK? ( -- bool )
   SWC 0 ?do
      SW-OUT i T-GET   SW-GATE i T-GET SW-UP i T-GET SWIGLU-F   f= 0= if false unloop exit then
   loop true ;
\ VJP exact goldens: read each input slot's accumulated gradient buffer and compare to the
\ closed forms d_gate = SWIGLU-DGATE(ct,gate,up), d_up = SWIGLU-DUP(ct,gate).
: SW-DGATE-OK? ( -- bool )
   0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ {: dg:ptr :}
   SWC 0 ?do
      dg i T-GET   SW-CT i T-GET SW-GATE i T-GET SW-UP i T-GET SWIGLU-DGATE  f= 0= if false unloop exit then
   loop true ;
: SW-DUP-OK? ( -- bool )
   1 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ {: du:ptr :}
   SWC 0 ?do
      du i T-GET   SW-CT i T-GET SW-GATE i T-GET SWIGLU-DUP  f= 0= if false unloop exit then
   loop true ;

\ shape-guard fixtures: same-shape gate/up is legal, a mismatched up is E-CAD-PARAM-SHAPE.
: SW-BAD ( -- )                              \ gate 2x3 vs up 1x3: not the same shape
   TENSOR:TV-RESET
   2 3 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   1 3 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   MAKI-OPKIND:SWIGLU EW-SHAPE-CHECK ;
: SW-OK ( -- )                               \ gate 2x3 vs up 2x3: legal
   TENSOR:TV-RESET
   2 3 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   2 3 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   MAKI-OPKIND:SWIGLU EW-SHAPE-CHECK ;
: SW-OK-THROWS ( -- n )  [: SW-OK ;] catch ;    \ positive control: a legal same-shape must NOT throw

\ device-lowering reject: analyze a captured swiglu region as an elementwise kernel
: SW-TRY-LEW ( -- )  0 FP-REGION-ID LEW-ANALYZE ;

T-RESET
SW-CT-FILL

\ ============================ (1) registry ====================================
MAKI-OPKIND:SWIGLU OPR-COMPLETE? TTRUE
MAKI-OPKIND:SWIGLU OPR-ARITY 2 T=
MAKI-OPKIND:SWIGLU OPR-NAME s" swiglu" STR= TTRUE

\ ============================ (2) capture =====================================
MODEL: SW-M ( gate:2x4 up:2x4 -- y ) SWIGLU ;
MODEL-K 1 T=                                                \ one op node
0 MIR-NODE-ID MIR-OP@ OPR-NAME s" swiglu" STR= TTRUE
0 MIR-NODE-ID MIR-IN-COUNT@ 2 T=                            \ arity 2 (gate, up)

\ ============================ (3) forward golden ==============================
MODEL: SW-FWD ( gate:2x4 up:2x4 -- y ) SWIGLU ;
SW-BIND-RUN
SW-FWD-OK? TTRUE                                            \ y == silu(gate)*up, element-exact

\ ============================ (4) shape guard (red-first) ====================
' SW-BAD E-CAD-PARAM-SHAPE TTHROWS
SW-OK-THROWS 0 T=                                           \ a legal same-shape does NOT throw

\ ============================ (5) VJP exact ==================================
MODEL: SW-VJP ( gate:2x4 up:2x4 -- y ) SWIGLU ;
BW-BUILD
EX-RESET
SW-FILL
SW-GATE 0 MIR-SLOT-ID EX-BIND
SW-UP   1 MIR-SLOT-ID EX-BIND
SW-CT BW-SEED-SLOT@ EX-BIND
EX-RUN
SW-DGATE-OK? TTRUE                                          \ d_gate == ct*up*silu'(gate) exactly
SW-DUP-OK? TTRUE                                            \ d_up   == ct*silu(gate)     exactly

\ ============================ (6) gradcheck =================================
MODEL: SW-GC ( gate:2x4 up:2x4 -- y ) SWIGLU ;
GC-RUN V-PASS T=                                            \ dx AND d-g match central FD
GC-RE$ s" 2 input(s) gradchecked" CONTAINS? TTRUE

\ ============================ (7) determinism: run-twice locks ===============
MODEL: SW-DET ( gate:2x4 up:2x4 -- y ) SWIGLU ;
SW-BIND-RUN  SW-OUT SW-Y1 SWC SW-SNAP
SW-BIND-RUN  SW-OUT SW-Y2 SWC SW-SNAP
SW-Y1 SW-Y2 SWC SW-ALL-EQ? TTRUE                            \ forward locked run-twice

\ ============================ (8) device lowering reject ====================
MODEL: SW-LOW ( gate:2x4 up:2x4 -- y ) SWIGLU ;
FP-BUILD
' SW-TRY-LEW E-LEW-OP TTHROWS                               \ no silent wrong device lowering

\ ============================ (9) integration ===============================
MODEL: SW-NET ( gate:2x4 up:2x4 -- y ) SWIGLU GELU ;
GC-RUN V-PASS T=                                            \ correct gradients flow through SwiGLU

T-REPORT

;package
