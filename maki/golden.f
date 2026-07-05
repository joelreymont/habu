\ maki/golden.f - the host GOLDEN verdict: external artifact OR self-consistency.
\
\ GOLDEN-INTO now has TWO legs (CAD-PLAN section 11). When an external reference
\ artifact exists for the current model (maki/golden-artifact.f), GOLDEN is a REAL
\ output-vs-reference comparison under the artifact's per-dtype tolerance (GA-CHECK).
\ Otherwise it falls back to the self-consistency v1 oracle this file owns: it runs the
\ captured forward IR as ONE composed chain (maki/executor.f) and, per node, RE-EXECUTES
\ that node from its (already composed) input buffers and checks the output is unchanged
\ - proving the composed walk and the per-node dispatch agree and that the executor is
\ deterministic and non-aliasing over the arena. Neither leg is yet the device-vs-host
\ comparison (that lands with the device leg; the reason/warn text says so honestly).
\
\ Verdict: PASS when every op is reference-complete + host-executable AND the composed
\ chain is self-consistent (or the artifact matches); NOT-RUN with a named reason when an
\ op is incomplete or not host-executable (cast / decode); FAIL only on a real mismatch.
\
\ The synthetic input binding and the host-executability membership predicate live one
\ file down in maki/golden-artifact.f (GA-BIND-SYNTH / GA-SUPPORTED? / GA-FIRST-BAD), so
\ the artifact reader and this oracle share them without a load-order cycle. One concern
\ here: the self-consistency verdict + the golden gate dispatch. maki -> habu; owns -5140.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/op-kind.f
require maki/op-registry.f
require maki/precision.f
require maki/model-ir.f
require maki/executor.f
require maki/report.f
require maki/golden-artifact.f

-5140 constant E-GOLD-CAP     \ golden per-node snapshot arena capacity exceeded

package MAKI
private

$1000 constant GO-SNAP-CELLS   \ per-node composed-output snapshot scratch
create GO-SNAP   GO-SNAP-CELLS cells allot

public
\ host-executability membership is owned by golden-artifact.f; expose the golden name.
: GO-SUPPORTED? ( -- bool )  GA-SUPPORTED? ;
private

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

\ ---- the host self-consistency verdict: V-PASS / V-FAIL / V-NOTRUN + reason ----
: GO-RUN ( -- n )
   MIR-N@ 0= if GO-RE-RESET s" golden: empty model" GO-RE+ V-NOTRUN exit then
   GA-FIRST-BAD {: bad:n :}
   bad 0< 0= if bad GO-REASON-BAD V-NOTRUN exit then
   GA-BIND-SYNTH
   MIR-N@ EX-RUN-N                                   \ composed forward chain
   GO-SELF? 0= if GO-RE-RESET s" golden: composed chain not self-consistent" GO-RE+ V-FAIL exit then
   GO-PASS-REASON  V-PASS ;

\ ---- golden leg marker (which leg produced the verdict) --------------------
\ GOLDEN-INTO here is the HOST gate: external reference artifact, else self-consistency. The
\ DEVICE model-golden leg is composed one layer up in maki/cad.f (GOLDEN-GATE-INTO), which owns
\ the device dependency; golden.f stays device-free. GOLDEN-DEV-FLAG records whether the device
\ leg produced the verdict so PROMOTE's evidence row can read golden=device-pass:<prec>, and
\ GOLDEN-PREC-V records the precision the device verdict was judged under (maki/precision.f);
\ the host legs here clear both (host legs carry no precision suffix).
variable GOLDEN-DEV-FLAG           \ -1 once the device golden leg produced the verdict, else 0
variable GOLDEN-PREC-V             \ precision id of the device golden verdict (PREC-F32 default)
public
: GOLDEN-DEV! ( n -- )  GOLDEN-DEV-FLAG ! ;
: GOLDEN-DEV? ( -- bool )  GOLDEN-DEV-FLAG @ 0= 0= ;
: GOLDEN-PREC! ( n -- )  GOLDEN-PREC-V ! ;
: GOLDEN-PREC@ ( -- n )  GOLDEN-PREC-V @ ;
private

\ ---- cad.f gate wiring: prefer an external reference artifact, else self-consistency --
: GO-GATE-ARTIFACT ( report -- report )
   GA-CHECK {: v:n :}  GA-RE$ v G-GOLDEN RPT-GATE!
   s" golden: external reference artifact comparison (per-artifact tolerance)" RPT-WARN+ ;
: GO-GATE-SELF ( report -- report )
   GO-RUN {: v:n :}  GO-RE$ v G-GOLDEN RPT-GATE!
   s" golden: host self-consistency (v1); device-vs-host is the cad.f device leg" RPT-WARN+ ;

public
: GOLDEN-INTO ( report -- report )              \ host golden gate (device leg is cad.f's GOLDEN-GATE-INTO)
   0 GOLDEN-DEV-FLAG !
   PREC-F32 GOLDEN-PREC-V !
   GA-EXISTS? if GO-GATE-ARTIFACT else GO-GATE-SELF then ;

end-package
