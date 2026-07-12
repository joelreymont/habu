\ maki/bcast.f - broadcast-operand classification for kernel load-index lowering.
\
\ The compute lowerings (maki/lower-ew.f, maki/lower-red.f) must load a region input
\ whose shape is a legal BROADCAST of the region shape (a 1xC bias, a 1x1 scale, an
\ Rx1 column) with the SAME element mapping the host executor uses, so the device
\ output matches the golden. This file is the single source of that mapping, mirroring
\ the executor reference EX-BC@ (maki/executor.f): a broadcast operand of shape br x bc
\ read against a target R x C at flat element e (r = e/C, c = e mod C) reads element
\   rr*bc + cc     where rr = (br==1)?0:r , cc = (bc==1)?0:c .
\ The four legal (each dim is 1-or-full) classes and their flat LOAD INDEX:
\   BC-FULL   (RxC): e            BC-COL    (Rx1): e / C
\   BC-ROW    (1xC): e mod C      BC-SCALAR (1x1): 0
\ Any dim that is neither 1 nor full is BC-ILLEGAL - the caller fails closed with its
\ own owned error code (E-LEW-BCAST / E-LRED-BCAST). Capture-time shape legality
\ (maki/cad.f SHP-LEGAL?) already restricts which class each op may broadcast, so this
\ classifier is the fail-closed re-derivation at lowering, not a policy of its own.

require maki/tensor.f

package MAKI
public

0  constant BC-FULL      \ br=R bc=C : the full region shape (no broadcast)
1  constant BC-ROW       \ br=1 bc=C : row-broadcast (1xC bias) -> load [e mod C]
2  constant BC-COL       \ br=R bc=1 : col-broadcast (Rx1)      -> load [e / C]
3  constant BC-SCALAR    \ br=1 bc=1 : scalar broadcast (1x1)   -> load [0]
-1 constant BC-ILLEGAL   \ a dim that is neither 1 nor full: not a legal broadcast

\ Classify an operand shape br x bc against the target R x C (fail-closed sentinel on
\ an illegal shape; the caller maps BC-ILLEGAL to its owned throw). FULL is tested first
\ so a degenerate R=1 or C=1 target (where classes coincide) picks the cheapest load.
: BC-CLASS ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- n )
   {: br:CAD-KIND:rows bc:CAD-KIND:cols r:CAD-KIND:rows c:CAD-KIND:cols :}
   br r ROWS-EQUAL? bc c COLS-EQUAL? and if BC-FULL   exit then
   br 1 ROWS-IS? bc c COLS-EQUAL? and if BC-ROW    exit then
   br r ROWS-EQUAL? bc 1 COLS-IS? and if BC-COL    exit then
   br 1 ROWS-IS? bc 1 COLS-IS? and if BC-SCALAR exit then
   BC-ILLEGAL ;

;package
