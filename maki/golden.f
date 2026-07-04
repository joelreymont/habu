\ maki/golden.f - the host GOLDEN self-consistency oracle (dot cad-7a, part D).
\
\ GOLDEN on host for reference-complete models (CAD-PLAN section 11): the executor
\ (maki/executor.f) IS the composition oracle, so GOLDEN executes the captured forward
\ IR as ONE composed chain and then, per node, RE-EXECUTES that node from its (already
\ composed) input buffers and checks the output is unchanged - a self-consistency v1
\ that proves the composed walk and the per-node dispatch agree and that the executor
\ is deterministic and non-aliasing over the arena. It is NOT yet the real device-vs-
\ host comparison: that lands when the device leg exists (the reason/warn text says so
\ honestly). Verdict: PASS when every op is reference-complete + host-executable AND the
\ composed chain is self-consistent; NOT-RUN with a named reason when an op is incomplete
\ or not host-executable (cast / decode); FAIL only if a node's re-execution disagrees.
\
\ Inputs are synthesized deterministically and bound through EX-BIND (index operands of a
\ gather are filled with valid row-0 indices). One concern: the golden self-consistency
\ verdict - it owns no planning, no gradients. maki -> habu only; golden owns -5140..-5142.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/op-kind.f
require maki/op-registry.f
require maki/model-ir.f
require maki/executor.f
require maki/report.f

-5140 constant E-GOLD-CAP     \ golden input / snapshot arena capacity exceeded

package MAKI
private

64    constant GO-SCAP        \ input slots (mirrors model-ir MIR-IN-CAP)
$2000 constant GO-ARENA-CELLS  \ synthetic input-buffer arena (float cells)
$1000 constant GO-SNAP-CELLS   \ per-node composed-output snapshot scratch

create GO-ARENA  GO-ARENA-CELLS cells allot
create GO-IN-OFF GO-SCAP cells allot
variable GO-BUMP
create GO-SNAP   GO-SNAP-CELLS cells allot

: GO-SLOT-ELEMS ( n -- n ) {: s:n :}  s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ * ;
: GO-IN-PTR ( n -- ptr a )  cells GO-IN-OFF + @ {: off:n :}  GO-ARENA off T-AT ;

\ ---- synthetic input fill (index operands get valid row-0 indices) ----------
: GO-NODE-IDX? ( n n -- bool ) {: nd:n s:n :}
   nd MIR-OP@ OP-GATHER <> if false exit then
   nd 1 MIR-IN@ {: r:n :}
   r MIR-REF-INPUT? 0= if false exit then
   r MIR-REF-SLOT s = ;

: GO-INDEX-SLOT? ( n -- bool ) {: s:n :}
   MIR-N@ 0 ?do  i s GO-NODE-IDX? if unloop true exit then  loop  false ;

: GO-FILL-VAL ( n n -- r ) {: s:n e:n :}
   s GO-INDEX-SLOT? if 0.0 exit then
   s 5 * e +  13 mod  s>f  0.17 f*  0.4 f+ ;

: GO-FILL-SLOT ( n -- ) {: s:n :}
   s GO-IN-PTR {: p:ptr :}
   s GO-SLOT-ELEMS 0 ?do  s i GO-FILL-VAL  p i T-SET  loop ;

: GO-BIND-INPUTS ( -- )
   EX-RESET
   0 GO-BUMP !
   MIR-IN-SLOTS@ 0 ?do
      i GO-SLOT-ELEMS {: e:n :}
      GO-BUMP @ {: off:n :}
      off e + GO-ARENA-CELLS > if E-GOLD-CAP throw then
      off i cells GO-IN-OFF + !
      off e + GO-BUMP !
      i GO-IN-PTR i EX-BIND
      i GO-FILL-SLOT
   loop ;

\ ---- membership gate: every node's op is reference-complete + host-executable -
public
: GO-SUPPORTED? ( -- bool )
   MIR-N@ 0 ?do
      i MIR-OP@ {: op:n :}
      op OPR-COMPLETE? 0=  op EX-OP-OK? 0= or  if false unloop exit then
   loop  true ;
private

: GO-FIRST-BAD ( -- n )        \ first node whose op blocks golden, or -1
   MIR-N@ 0 ?do
      i MIR-OP@ {: op:n :}
      op OPR-COMPLETE? 0=  op EX-OP-OK? 0= or  if i unloop drop op exit then
   loop  -1 ;

\ ---- per-node self-consistency: composed output == re-exec from its inputs ---
: GO-DIFF? ( r r -- bool ) {: a:r b:r :}  a b f- fabs  0.000001  f< 0= ;

: GO-NODE-OK? ( n -- bool ) {: nd:n :}
   nd EX-NODE-ELEMS {: e:n :}
   e GO-SNAP-CELLS > if E-GOLD-CAP throw then
   nd EX-OUT@ {: op:ptr :}
   e 0 ?do  op i T-GET  GO-SNAP i T-SET  loop        \ snapshot composed output
   nd EX-NODE                                         \ re-execute from its inputs
   e 0 ?do  op i T-GET  GO-SNAP i T-GET  GO-DIFF? if false unloop exit then  loop
   true ;

: GO-SELF? ( -- bool )
   MIR-N@ 0 ?do  i GO-NODE-OK? 0= if false unloop exit then  loop  true ;

\ ---- reason buffer ---------------------------------------------------------
128 constant GO-RE-CAP
create GO-RE GO-RE-CAP allot  variable GO-RE-U
: GO-RE-RESET ( -- )  0 GO-RE-U ! ;
: GO-RE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   GO-RE-U @ u + GO-RE-CAP > if exit then
   a GO-RE GO-RE-U @ + u BYTE-COPY  GO-RE-U @ u + GO-RE-U ! ;
: GO-RE-INT ( n -- )  SB-RESET SB-INT SB$ GO-RE+ ;
public
: GO-RE$ ( -- ptr u8 n )  GO-RE GO-RE-U @ ;
private

: GO-REASON-BAD ( n -- ) {: op:n :}
   GO-RE-RESET
   op OPR-COMPLETE? 0= if s" golden: incomplete op " GO-RE+ op OPR-NAME GO-RE+ exit then
   s" golden: op not host-executable " GO-RE+ op OPR-NAME GO-RE+ ;

: GO-PASS-REASON ( -- )
   GO-RE-RESET s" host self-consistent (" GO-RE+ MIR-N@ GO-RE-INT
   s"  nodes); device-vs-host pending" GO-RE+ ;

public

\ ---- the host golden verdict: V-PASS / V-FAIL / V-NOTRUN + reason in GO-RE$ ---
: GO-RUN ( -- n )
   MIR-N@ 0= if GO-RE-RESET s" golden: empty model" GO-RE+ V-NOTRUN exit then
   GO-FIRST-BAD {: bad:n :}
   bad 0< 0= if bad GO-REASON-BAD V-NOTRUN exit then
   GO-BIND-INPUTS
   MIR-N@ EX-RUN-N                                   \ composed forward chain
   GO-SELF? 0= if GO-RE-RESET s" golden: composed chain not self-consistent" GO-RE+ V-FAIL exit then
   GO-PASS-REASON  V-PASS ;

\ ---- cad.f gate wiring ------------------------------------------------------
: GOLDEN-INTO ( report -- report )
   GO-RUN {: v:n :}
   GO-RE$ v G-GOLDEN RPT-GATE!
   s" golden: host self-consistency (v1); device-vs-host lands with the device leg" RPT-WARN+ ;

end-package
