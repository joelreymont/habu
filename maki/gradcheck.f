\ maki/gradcheck.f - host numeric gradcheck over the model IR (CAD-PLAN 12; cad-7a).
\
\ Makes GRADCHECK REAL on the host at TENSOR granularity by driving the full-tensor
\ executor (maki/executor.f): it builds the backward IR (maki/backward.f), binds host
\ buffers for every model input, runs the WHOLE IR (forward chain + emitted backward
\ region) with EX-RUN, reads each input's ANALYTIC gradient out of its backward node
\ buffer (BW-SLOT-GRAD@), and compares it against a CENTRAL finite difference of the
\ scalar loss L = sum_k seed_k * output_k - exactly the loss whose cotangent the
\ backward pass seeds. So every host-executable, reference-complete op now gradchecks
\ (MLP = LINEAR GELU LINEAR, norms, softmax, rope, movement, reduce/scatter); only an
\ op with no host reference (cast / decode) or no supported adjoint stays honestly
\ NOT-RUN with a named reason (never a false pass).
\
\ FD sampling policy: perturbing every element of every input is O(elems * evals), so a
\ per-input SAMPLE (element 0, the last element, and the middle) is checked - the corners
\ plus the midpoint of each input buffer. That keeps runtime bounded while still catching
\ a wrong adjoint (the detection fixture in the test proves it). The output cotangent is
\ seeded with a varied non-uniform pattern so a softmax row (whose plain-sum gradient is
\ identically zero) still exercises a real gradient.
\
\ It brackets BW-BUILD with MIR-MARK/MIR-RELEASE so the throwaway backward pass never
\ leaks into the shared model IR, and index inputs (gather) are filled with valid (zero)
\ indices so gather/scatter forward+backward run. maki -> habu only; gradcheck owns
\ -5115..-5119.

require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/op-kind.f
require maki/op-registry.f
require maki/model-ir.f
require maki/adjoint.f
require maki/backward.f
require maki/executor.f
require maki/report.f

-5115 constant E-GC-EXEC     \ reserved: a node op-kind is not host-executable (gated)
-5116 constant E-GC-CAP      \ input-buffer arena capacity exceeded

package MAKI
private

64    constant GC-SCAP        \ input slots (mirrors model-ir MIR-IN-CAP)
$4000 constant GC-ARENA-CELLS  \ host input-buffer arena (float cells)

create GC-ARENA  GC-ARENA-CELLS cells allot   \ per-input-slot host buffers
create GC-IN-OFF GC-SCAP cells allot           \ per-slot arena offset (cells)
variable GC-BUMP
1 LAYOUT-BUFFER GC-MARK-BUF MIR:mark

: GC-MARK! ( MIR:mark -- )  0 GC-MARK-BUF ! ;
: GC-MARK@ ( -- MIR:mark )  0 GC-MARK-BUF @ ;

: GC-H ( -- r )  0.001 ;                       \ central finite-difference step

public
\ analytic vs finite-diff agreement (absolute + 1% relative tolerance)
: GC-CLOSE? ( r r -- bool ) {: a:r b:r :}
   a b f- fabs   0.01  0.01 a fabs f* f+   f< ;
private

\ ---- reason buffer (dynamic gate reason: named op / mismatch slot) -----------
128 constant GC-RE-CAP
create GC-RE GC-RE-CAP allot  variable GC-RE-U
: GC-RE-RESET ( -- )  0 GC-RE-U ! ;
: GC-RE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   GC-RE-U @ u + GC-RE-CAP > if exit then         \ reason is advisory; never overflow
   a GC-RE GC-RE-U @ + u BYTE-COPY  GC-RE-U @ u + GC-RE-U ! ;
: GC-RE-INT ( n -- )  SB-RESET SB-INT SB$ GC-RE+ ;
public
: GC-RE$ ( -- ptr u8 n )  GC-RE GC-RE-U @ ;
private

\ ---- input-slot host buffers -----------------------------------------------
: GC-SLOT-ELEMS ( MIR:input-slot -- n ) {: s:MIR:input-slot :}
   s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ SHAPE-ELEMS DIM-RAW ;
: GC-IN-PTR ( MIR:input-slot -- ptr a )
   SLOT>RAW cells GC-IN-OFF + @ {: off:n :}  GC-ARENA off T-AT ;

\ deterministic non-degenerate fills (relu-safe: strictly positive, varied)
: GC-INPUT-VAL ( MIR:input-slot n -- r ) {: s:MIR:input-slot e:n :}
   s SLOT>RAW 7 * e + 17 mod s>f 0.11 f* 0.3 f+ ;
: GC-SEED-VAL  ( n -- r )   {: e:n :}      e 7 mod  s>f  0.13 f*  0.6 f+ ;

\ an input slot is an index slot if a forward gather reads it as its index operand
: GC-NODE-IDX? ( CAD-KIND:node-id MIR:input-slot -- bool )
   {: nd:CAD-KIND:node-id s:MIR:input-slot :}
   nd MIR-OP@ MAKI-OPKIND:GATHER MAKI-OPKIND:EQ 0= if false exit then
   nd 1 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? 0= if false exit then
   r MIR-REF-SLOT s MIR-SLOT= ;

: GC-INDEX-SLOT? ( MIR:input-slot -- bool ) {: s:MIR:input-slot :}
   BW-FWD-N@ 0 ?do
      i MIR-NODE-ID s GC-NODE-IDX? if unloop true exit then
   loop false ;

: GC-FILL-VAL ( MIR:input-slot n -- r ) {: s:MIR:input-slot e:n :}   \ value for slot s element e
   s BW-SEED-SLOT@ MIR-SLOT= if e GC-SEED-VAL exit then
   s GC-INDEX-SLOT? if 0.0 exit then           \ index slot: valid (row 0) indices
   s e GC-INPUT-VAL ;

: GC-FILL-SLOT ( MIR:input-slot -- ) {: s:MIR:input-slot :}
   s GC-IN-PTR {: p:ptr :}
   s GC-SLOT-ELEMS 0 ?do  s i GC-FILL-VAL  p i T-SET  loop ;

\ carve + bind + fill a host buffer for every current input slot (seed included)
: GC-BIND-INPUTS ( -- )
   EX-RESET
   0 GC-BUMP !
   MIR-IN-SLOTS@ 0 ?do
      i MIR-SLOT-ID {: s:MIR:input-slot :}
      s GC-SLOT-ELEMS {: e:n :}
      GC-BUMP @ {: off:n :}
      off e + GC-ARENA-CELLS > if E-GC-CAP throw then
      off s SLOT>RAW cells GC-IN-OFF + !
      off e + GC-BUMP !
      s GC-IN-PTR s EX-BIND
      s GC-FILL-SLOT
   loop ;

\ ---- loss L = sum_k seed_k * output_k (the backward's seeded loss) -----------
: GC-OUT-SUM ( -- r )
   BW-FWD-N@ 1- MIR-NODE-ID {: out:CAD-KIND:node-id :}
   out EX-OUT@ {: op:ptr :}
   BW-SEED-SLOT@ GC-IN-PTR {: sp:ptr :}
   0.0  out EX-NODE-ELEMS 0 ?do  op i T-GET  sp i T-GET  f*  f+  loop ;

\ ---- analytic gradient element = the backward node's output element ----------
: GC-ANALYTIC-EL ( MIR:input-slot n -- r ) {: s:MIR:input-slot e:n :}
   s BW-SLOT-GRAD@ {: g:MIR:operand-ref :}
   g MIR-REF-INPUT? if g MIR-REF-SLOT GC-IN-PTR e T-GET
   else g MIR-REF-NODE EX-OUT@ e T-GET then ;

\ ---- central finite difference dL/d(input s element e) over the forward slice -
: GC-FD-SUM ( MIR:input-slot n -- r ) {: s:MIR:input-slot e:n :}
   s GC-IN-PTR {: p:ptr :}
   p e T-GET {: base:r :}
   base GC-H f+ p e T-SET  BW-FWD-N@ EX-RUN-N  GC-OUT-SUM {: yp:r :}
   base GC-H f- p e T-SET  BW-FWD-N@ EX-RUN-N  GC-OUT-SUM {: ym:r :}
   base p e T-SET
   yp ym f-  GC-H 2.0 f* f/ ;

\ one sampled element: full run (analytic), then finite-diff the forward slice
: GC-SAMPLE-OK? ( MIR:input-slot n -- bool ) {: s:MIR:input-slot e:n :}
   MIR-N@ EX-RUN-N
   s e GC-ANALYTIC-EL {: a:r :}
   s e GC-FD-SUM {: fd:r :}
   a fd GC-CLOSE? ;

\ sample the corners + middle of an input buffer (bounded runtime)
: GC-CHECK-SLOT? ( MIR:input-slot -- bool ) {: s:MIR:input-slot :}
   s GC-SLOT-ELEMS {: e:n :}
   s 0 GC-SAMPLE-OK? 0= if false exit then
   e 1 > if s e 1- GC-SAMPLE-OK? 0= if false exit then then
   e 2 > if s e 2 / GC-SAMPLE-OK? 0= if false exit then then
   true ;

\ ---- verdict reasons -------------------------------------------------------
: GC-FAIL-REASON ( MIR:input-slot -- ) {: s:MIR:input-slot :}
   GC-RE-RESET s" host: input " GC-RE+ s SLOT>RAW GC-RE-INT
   s"  analytic != finite-diff" GC-RE+ ;
: GC-PASS-REASON ( -- )
   GC-RE-RESET s" host: " GC-RE+ BW-SEED-SLOT@ SLOT>RAW GC-RE-INT
   s"  input(s) gradchecked" GC-RE+ ;

\ check every model data slot that received a gradient; V-PASS or V-FAIL (named slot)
: GC-CHECK-ALL ( -- n )
   BW-SEED-SLOT@ SLOT>RAW 0 ?do
      i MIR-SLOT-ID {: s:MIR:input-slot :}
      s BW-HAS-GRAD? if
         s GC-CHECK-SLOT? 0= if s GC-FAIL-REASON V-FAIL unloop exit then
      then
   loop
   GC-PASS-REASON  V-PASS ;

\ ---- blocking-op classification (only cast / no-adjoint / unsupported remain) -
\ first blocking NODE index (op family refetched at the use site), or -1
: GC-FIRST-BAD ( -- n )
   MIR-N@ 0 ?do
      i MIR-NODE-ID MIR-OP@ dup ADJ-HAS? 0=  swap dup ADJ-SUP? 0=  swap EX-OP-OK? 0=  or or
      if i unloop exit then
   loop  -1 ;

: GC-REASON-BAD ( n -- ) {: raw:n :}
   raw MIR-NODE-ID {: nd:CAD-KIND:node-id :}
   GC-RE-RESET
   nd MIR-OP@ ADJ-HAS? 0=  if s" no-adjoint:" GC-RE+          nd MIR-OP@ OPR-NAME GC-RE+ exit then
   nd MIR-OP@ ADJ-SUP? 0=  if s" unsupported-adjoint:" GC-RE+ nd MIR-OP@ OPR-NAME GC-RE+ exit then
   s" host-unsupported:" GC-RE+  nd MIR-OP@ OPR-NAME GC-RE+ ;

public

\ ---- the host gradcheck: verdict (V-PASS/V-FAIL/V-NOTRUN) + reason in GC-RE$ ---
: GC-RUN ( -- n )
   MIR-N@ 0= if GC-RE-RESET s" host: empty model" GC-RE+ V-NOTRUN exit then
   GC-FIRST-BAD {: bad:n :}
   bad 0< 0= if bad GC-REASON-BAD V-NOTRUN exit then
   MIR-MARK GC-MARK!                              \ restore-mark (nodes slots ins-u)
   BW-BUILD
   GC-BIND-INPUTS
   GC-CHECK-ALL {: v:n :}
   GC-MARK@ MIR-RELEASE                            \ drop the throwaway backward pass
   v ;

\ ---- cad.f gate wiring ------------------------------------------------------
: GRADCHECK-INTO ( report -- report )
   GC-RUN {: v:n :}  GC-RE$ v G-GRADCHECK REPORT:GATE! ;

;package
