\ maki/gradcheck.f - host numeric gradcheck over the model IR (CAD-PLAN 12, dot cad-9d).
\
\ Makes GRADCHECK REAL on the host (an honest upgrade from not-run) for models whose
\ ops are all reference-complete AND host-executable: it builds the backward IR
\ (maki/backward.f), runs the FORWARD reference chain over tiny seeded host buffers,
\ reads the ANALYTIC input gradients straight out of the backward IR nodes (executing
\ their scalar references), and compares them against CENTRAL finite differences of the
\ forward chain (the maki/autograd.f discipline). A per-element scalar sample is exact
\ for the elementwise op set, which is the v1 host-executable class; reductions /
\ matmul / rope need the cad-7 full-tensor executor, so a model containing one is
\ NOT-RUN with a named reason (never a false pass). Ops with no adjoint -> not-run
\ `no-adjoint:<op>`, per the registry contract.
\
\ The executor dispatches op-kind to the KNOWN scalar word (a checked case), never an
\ untyped stored xt. It brackets BW-BUILD with MIR-MARK/MIR-RELEASE so the throwaway
\ backward pass never leaks into the shared model IR. This word is standalone so the
\ cad-7 executor can reuse it. maki -> habu only; gradcheck owns -5115..-5119.

require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/op-kind.f
require maki/op-registry.f
require maki/model-ir.f
require maki/adjoint.f
require maki/backward.f
require maki/gelu.f
require maki/silu.f
require maki/autograd.f
require maki/report.f

-5115 constant E-GC-EXEC     \ a node op-kind is not host-executable (executor bug: gated)

package MAKI
private

128 constant GC-NCAP        \ per-node value slots (mirrors model-ir MIR-CAP)
64  constant GC-SCAP        \ per-input sample slots (mirrors model-ir MIR-IN-CAP)

create GC-VF  GC-NCAP cells allot     \ per-node computed value (float cell)
create GC-IS  GC-SCAP cells allot      \ per-input-slot sample value (float cell)

: GC-H ( -- r )  0.001 ;              \ central finite-difference step

public
: GC-VF@ ( n -- r )  cells GC-VF + @ ;
: GC-VF! ( r n -- )  cells GC-VF + ! ;
: GC-IS@ ( n -- r )  cells GC-IS + @ ;
: GC-IS! ( r n -- )  cells GC-IS + ! ;
private

\ ---- host-executable op set (elementwise; a scalar sample is exact) ----------
: GC-EXEC-OP? ( n -- bool ) {: op:n :}
   op OP-RELU = op OP-GELU = or op OP-SILU = or
   op OP-ADD = or op OP-RESIDUAL-ADD = or op OP-MUL = or
   op OP-RELU-BWD = or op OP-GELU-BWD = or op OP-SILU-BWD = or ;

\ operand value: a model input reads its sample, a node reads its computed value
: GC-OPND ( n -- r ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT GC-IS@ else ref GC-VF@ then ;

\ ---- apply one node's op to its operand values (checked per-branch effect) ----
: GC-APPLY ( n -- r ) {: nd:n :}
   nd 0 MIR-IN@ GC-OPND {: a:r :}
   nd MIR-OP@ case
      OP-RELU         of a RELU-F endof
      OP-GELU         of a GELU-F endof
      OP-SILU         of a SILU-F endof
      OP-ADD          of a  nd 1 MIR-IN@ GC-OPND  f+ endof
      OP-RESIDUAL-ADD of a  nd 1 MIR-IN@ GC-OPND  f+ endof
      OP-MUL          of a  nd 1 MIR-IN@ GC-OPND  f* endof
      OP-RELU-BWD     of a  nd 1 MIR-IN@ GC-OPND  RELU-BWD endof
      OP-GELU-BWD     of a  nd 1 MIR-IN@ GC-OPND  GELU-BWD endof
      OP-SILU-BWD     of a  nd 1 MIR-IN@ GC-OPND  SILU-BWD endof
      E-GC-EXEC throw
   endcase ;

public
\ evaluate nodes 0..n-1 into GC-VF (forward-only pass when n = forward count)
: GC-EVAL-N ( n -- ) {: n:n :}
   n 0 ?do  i GC-APPLY  i GC-VF!  loop ;

\ deterministic non-zero seed fill for every current input slot (avoids the relu kink)
: GC-FILL ( -- )
   MIR-IN-SLOTS@ 0 ?do  i s>f 0.3 f* 0.5 f+  i GC-IS!  loop ;

\ central finite difference d(out)/d(slot): perturb, re-run the forward chain, restore
: GC-FD ( n n n -- r ) {: s:n fwd:n out:n :}
   s GC-IS@ {: base:r :}
   base GC-H f+ s GC-IS!  fwd GC-EVAL-N  out GC-VF@ {: yp:r :}
   base GC-H f- s GC-IS!  fwd GC-EVAL-N  out GC-VF@ {: ym:r :}
   base s GC-IS!
   yp ym f-  GC-H 2.0 f* f/ ;

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

\ first forward op-kind that blocks host gradcheck, or -1 (scans the forward IR)
: GC-FIRST-BAD ( -- n )
   MIR-N@ 0 ?do
      i MIR-OP@ {: op:n :}
      op ADJ-HAS? 0=  op ADJ-SUP? 0= or  op GC-EXEC-OP? 0= or
      if i unloop drop op exit then
   loop  -1 ;

\ classify a blocking op into the gate reason
: GC-REASON-BAD ( n -- ) {: op:n :}
   GC-RE-RESET
   op ADJ-HAS? 0=      if s" no-adjoint:" GC-RE+          op OPR-NAME GC-RE+ exit then
   op ADJ-SUP? 0=      if s" unsupported-adjoint:" GC-RE+ op OPR-NAME GC-RE+ exit then
   s" host-unsupported:" GC-RE+  op OPR-NAME GC-RE+ ;

\ ---- analytic input gradient = the backward node that produced the slot's grad ---
: GC-ANALYTIC ( n -- r ) {: s:n :}
   s BW-SLOT-GRAD@ {: g:n :}
   g MIR-REF-INPUT? if g MIR-REF-SLOT GC-IS@ else g GC-VF@ then ;

\ one data slot: full eval (analytic), then finite-diff the forward chain
: GC-SLOT-OK? ( n -- bool ) {: s:n :}
   MIR-N@ GC-EVAL-N
   s GC-ANALYTIC {: a:r :}
   s BW-FWD-N@  BW-FWD-N@ 1-  GC-FD {: fd:r :}
   a fd GC-CLOSE? ;

\ check every model data slot that received a gradient; V-PASS or V-FAIL (named slot)
: GC-CHECK-ALL ( -- n )
   BW-SEED-SLOT@ 0 ?do
      i BW-HAS-GRAD? if
         i GC-SLOT-OK? 0= if
            GC-RE-RESET s" host: input " GC-RE+ i GC-RE-INT s"  analytic != finite-diff" GC-RE+
            V-FAIL unloop exit
         then
      then
   loop
   GC-RE-RESET s" host: " GC-RE+ BW-SEED-SLOT@ GC-RE-INT s"  input(s) gradchecked" GC-RE+
   V-PASS ;

public

\ ---- the host gradcheck: verdict (V-PASS/V-FAIL/V-NOTRUN) + reason in GC-RE$ ---
: GC-RUN ( -- n )
   MIR-N@ 0= if GC-RE-RESET s" host: empty model" GC-RE+ V-NOTRUN exit then
   GC-FIRST-BAD {: bad:n :}
   bad 0< 0= if bad GC-REASON-BAD V-NOTRUN exit then
   MIR-MARK {: nn:n sn:n iu:n :}                  \ restore-marks (nodes slots ins-u)
   BW-BUILD
   GC-FILL  1.0 BW-SEED-SLOT@ GC-IS!               \ seed the output cotangent = 1
   GC-CHECK-ALL {: v:n :}
   nn sn iu MIR-RELEASE                            \ drop the throwaway backward pass
   v ;

\ ---- cad.f gate wiring ------------------------------------------------------
: GRADCHECK-INTO ( report -- report )
   GC-RUN {: v:n :}  GC-RE$ v G-GRADCHECK RPT-GATE! ;

end-package
