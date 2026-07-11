\ maki/executor.f - the full-tensor host executor for the model IR (dot cad-7a).
\
\ Executes the captured model-IR node table (maki/model-ir.f) at TENSOR granularity
\ over host float-cell buffers - the GOLDEN composition oracle (CAD-PLAN section 11)
\ and the numeric engine the host gradcheck (maki/gradcheck.f) drives. It walks the
\ nodes in INDEX (topo) order (a node only references earlier nodes / input slots),
\ allocates one contiguous float-cell buffer per node from its declared shape out of
\ a fixed arena, and dispatches each op-kind to its buffer-level reference:
\   elementwise  - the scalar ops (maki/autograd.f, gelu/silu.f) mapped over elements,
\                  with 1xC / 1x1 / RxC broadcast on the second operand (bias/scale);
\   row words    - layernorm/rmsnorm/softmax + their VJPs applied per row over C cols;
\   matmul/linear- the contraction references (maki/matmul.f, maki/linear.f);
\   movement     - reshape/transpose/slice/concat/gather (maki/move.f) with attrs;
\   reduce/scatter backward - rowsum/fullsum-dot/pad-scatter/scatter-add (maki/
\                  reduce-bwd.f, maki/scatter.f);
\   rope         - ROPE-PAIR / ROPE-BWD applied over adjacent column pairs.
\
\ Rope pairing convention: cos/sin operands are the SAME RxC shape as x; for the pair
\ at columns (2k, 2k+1) the executor reads cos[row,2k] and sin[row,2k] (the pair's base
\ column). C must be even (an odd trailing column is left untouched). ROPE-BWD is the
\ exact transpose of ROPE-PAIR for ANY (cos,sin), so it is a valid VJP without a unit
\ rotation.
\
\ Model-input buffers are supplied by the caller (EX-BIND), validated against the slot
\ table; node buffers are the executor's own arena. One concern: EXECUTION ONLY - no
\ planning, no gradients, no reporting. Fail closed: an op with no host reference
\ (cast / decode) is E-EX-UNSUP; an unbound input is E-EX-UNBOUND; arena / index
\ overflow is E-EX-CAP; a bad slot / node index is E-EX-SLOT / E-EX-NODE. Gather
\ indices are read from the float operand (rounded to nearest) into an int scratch, so
\ the exact move.f gather/scatter references run unchanged. maki -> habu; owns -5130..-5134.

require lib/string.f
require lib/float.f
require maki/array.f
require maki/op-kind.f
require maki/op-registry.f
require maki/move-facts.f
require maki/model-ir.f
require maki/autograd.f
require maki/gelu.f
require maki/silu.f
require maki/layernorm.f
require maki/rmsnorm.f
require maki/softmax.f
require maki/rope.f
require maki/matmul.f
require maki/linear.f
require maki/move.f
require maki/reduce-bwd.f
require maki/scatter.f

-5130 constant E-EX-CAP       \ node arena / index scratch capacity exceeded
-5131 constant E-EX-UNSUP     \ op-kind has no host-executable reference (cast / decode)
-5132 constant E-EX-UNBOUND   \ an operand names a model-input slot with no bound buffer
-5133 constant E-EX-SLOT      \ model-input slot index out of range
-5134 constant E-EX-NODE      \ node index out of range (EX-OUT@)

package MAKI
private

\ node / slot caps mirror model-ir.f (MIR-CAP / MIR-IN-CAP, private there).
128   constant EX-NCAP        \ max nodes (mirrors MIR-CAP)
64    constant EX-IN-CAP      \ max model-input slots (mirrors MIR-IN-CAP)
$8000 constant EX-ARENA-CELLS \ node-buffer arena (float cells); overflow -> E-EX-CAP
$400  constant EX-IDX-CAP     \ gather/scatter index scratch (rows)

create EX-ARENA  EX-ARENA-CELLS cells allot   \ contiguous node-buffer pool
create EX-OFF    EX-NCAP cells allot           \ per-node arena offset (in cells)
variable EX-BUMP                                \ arena high-water (cells) during a plan
create EX-IN-PTR EX-IN-CAP cells allot          \ per-slot bound buffer pointer
create EX-IN-SET EX-IN-CAP cells allot          \ per-slot bound flag (0 = unbound)
create EX-IDX    EX-IDX-CAP cells allot          \ int index scratch (gather/scatter)

\ ---- slot / node addressing ------------------------------------------------
: EX-IN-CK ( n -- n )                   \ validate a model-input slot index
   dup 0 < over MIR-IN-SLOTS@ >= or if E-EX-SLOT throw then ;

: EX-SLOT-PTR ( n -- ptr a ) {: s:n :}  \ bound buffer for an input slot (fail closed)
   s EX-IN-CK drop
   s cells EX-IN-SET + @ 0= if E-EX-UNBOUND throw then
   EX-IN-PTR s cells + @ ;

: EX-OFF@ ( n -- n )        cells EX-OFF + @ ;
: EX-NODE-PTR ( n -- ptr a )  EX-OFF@ {: off:n :}  EX-ARENA off T-AT ;

\ ---- operand-ref descriptor (input slot or producer node) -------------------
: EX-REF-PTR ( n -- ptr a ) {: r:n :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT EX-SLOT-PTR else r EX-NODE-PTR then ;
: EX-REF-ROWS ( n -- n ) {: r:n :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-ROWS@ then ;
: EX-REF-COLS ( n -- n ) {: r:n :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-COLS@ then ;
: EX-REF-ELEMS ( n -- n ) {: r:n :}  r EX-REF-ROWS  r EX-REF-COLS  * ;

public
: EX-NODE-ELEMS ( n -- n ) {: nd:n :}  nd MIR-ROWS@ nd MIR-COLS@ * ;
private

\ ---- elementwise: scalar reference mapped over elements ---------------------
\ the op family is on top (the element value below it); exhaustive MATCH, so an
\ op with no unary scalar reference is a checker-forced explicit throw.
: EX-U-EL ( r opkind -- r )
   MATCH opkind
      relu OF RELU-F ENDOF  gelu OF GELU-F ENDOF  silu OF SILU-F ENDOF
      add OF E-EX-UNSUP throw ENDOF  mul OF E-EX-UNSUP throw ENDOF
      scale OF E-EX-UNSUP throw ENDOF  bias OF E-EX-UNSUP throw ENDOF
      layernorm OF E-EX-UNSUP throw ENDOF  rmsnorm OF E-EX-UNSUP throw ENDOF
      softmax-row OF E-EX-UNSUP throw ENDOF  matmul OF E-EX-UNSUP throw ENDOF
      linear OF E-EX-UNSUP throw ENDOF  residual-add OF E-EX-UNSUP throw ENDOF
      cast OF E-EX-UNSUP throw ENDOF  rope OF E-EX-UNSUP throw ENDOF
      reshape OF E-EX-UNSUP throw ENDOF  transpose OF E-EX-UNSUP throw ENDOF
      slice OF E-EX-UNSUP throw ENDOF  concat OF E-EX-UNSUP throw ENDOF
      gather OF E-EX-UNSUP throw ENDOF  relu-bwd OF E-EX-UNSUP throw ENDOF
      gelu-bwd OF E-EX-UNSUP throw ENDOF  silu-bwd OF E-EX-UNSUP throw ENDOF
      layernorm-bwd OF E-EX-UNSUP throw ENDOF  rmsnorm-bwd OF E-EX-UNSUP throw ENDOF
      softmax-row-bwd OF E-EX-UNSUP throw ENDOF  rope-bwd OF E-EX-UNSUP throw ENDOF
      rowsum-bwd OF E-EX-UNSUP throw ENDOF  fullsum-dot-bwd OF E-EX-UNSUP throw ENDOF
      pad-scatter OF E-EX-UNSUP throw ENDOF  scatter-add OF E-EX-UNSUP throw ENDOF
   ;MATCH ;

: EX-U ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ EX-REF-PTR {: ap:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-ELEMS 0 ?do  ap i T-GET  nd MIR-OP@ EX-U-EL  ob i T-SET  loop ;

\ broadcast read b[r,c]: a 1-row operand reads row 0, a 1-col operand reads col 0.
: EX-BC@ ( ptr a n n n n -- r ) {: bp:ptr br:n bc:n r:n c:n :}
   br 1 = if 0 else r then {: rr:n :}
   bc 1 = if 0 else c then {: cc:n :}
   bp  rr bc *  cc +  T-GET ;

: EX-EW2-EL ( r r opkind -- r )
   MATCH opkind
      add OF ADD-F ENDOF  residual-add OF ADD-F ENDOF  bias OF ADD-F ENDOF
      mul OF MUL-F ENDOF  scale OF MUL-F ENDOF
      relu-bwd OF RELU-BWD ENDOF  gelu-bwd OF GELU-BWD ENDOF  silu-bwd OF SILU-BWD ENDOF
      relu OF E-EX-UNSUP throw ENDOF  gelu OF E-EX-UNSUP throw ENDOF
      silu OF E-EX-UNSUP throw ENDOF  layernorm OF E-EX-UNSUP throw ENDOF
      rmsnorm OF E-EX-UNSUP throw ENDOF  softmax-row OF E-EX-UNSUP throw ENDOF
      matmul OF E-EX-UNSUP throw ENDOF  linear OF E-EX-UNSUP throw ENDOF
      cast OF E-EX-UNSUP throw ENDOF  rope OF E-EX-UNSUP throw ENDOF
      reshape OF E-EX-UNSUP throw ENDOF  transpose OF E-EX-UNSUP throw ENDOF
      slice OF E-EX-UNSUP throw ENDOF  concat OF E-EX-UNSUP throw ENDOF
      gather OF E-EX-UNSUP throw ENDOF  layernorm-bwd OF E-EX-UNSUP throw ENDOF
      rmsnorm-bwd OF E-EX-UNSUP throw ENDOF  softmax-row-bwd OF E-EX-UNSUP throw ENDOF
      rope-bwd OF E-EX-UNSUP throw ENDOF  rowsum-bwd OF E-EX-UNSUP throw ENDOF
      fullsum-dot-bwd OF E-EX-UNSUP throw ENDOF  pad-scatter OF E-EX-UNSUP throw ENDOF
      scatter-add OF E-EX-UNSUP throw ENDOF
   ;MATCH ;

: EX-EW2 ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ EX-REF-PTR {: ap:ptr :}
   nd 1 MIR-IN@ {: bref:n :}
   bref EX-REF-PTR {: bp:ptr :}  bref EX-REF-ROWS {: br:n :}  bref EX-REF-COLS {: bc:n :}
   nd EX-NODE-PTR {: ob:ptr :}  nd MIR-COLS@ {: C:n :}
   nd EX-NODE-ELEMS 0 ?do
      ap i T-GET   bp br bc  i C /  i C mod  EX-BC@   nd MIR-OP@ EX-EW2-EL   ob i T-SET
   loop ;

\ ---- row words (layernorm / rmsnorm / softmax + VJPs) applied per row -------
: EX-ROW-FWD-1 ( ptr a ptr a n opkind -- )
   MATCH opkind
      layernorm OF LN-FWD ENDOF  rmsnorm OF RMS-FWD ENDOF  softmax-row OF SM-FWD ENDOF
      add OF E-EX-UNSUP throw ENDOF  mul OF E-EX-UNSUP throw ENDOF
      scale OF E-EX-UNSUP throw ENDOF  bias OF E-EX-UNSUP throw ENDOF
      relu OF E-EX-UNSUP throw ENDOF  gelu OF E-EX-UNSUP throw ENDOF
      silu OF E-EX-UNSUP throw ENDOF  matmul OF E-EX-UNSUP throw ENDOF
      linear OF E-EX-UNSUP throw ENDOF  residual-add OF E-EX-UNSUP throw ENDOF
      cast OF E-EX-UNSUP throw ENDOF  rope OF E-EX-UNSUP throw ENDOF
      reshape OF E-EX-UNSUP throw ENDOF  transpose OF E-EX-UNSUP throw ENDOF
      slice OF E-EX-UNSUP throw ENDOF  concat OF E-EX-UNSUP throw ENDOF
      gather OF E-EX-UNSUP throw ENDOF  relu-bwd OF E-EX-UNSUP throw ENDOF
      gelu-bwd OF E-EX-UNSUP throw ENDOF  silu-bwd OF E-EX-UNSUP throw ENDOF
      layernorm-bwd OF E-EX-UNSUP throw ENDOF  rmsnorm-bwd OF E-EX-UNSUP throw ENDOF
      softmax-row-bwd OF E-EX-UNSUP throw ENDOF  rope-bwd OF E-EX-UNSUP throw ENDOF
      rowsum-bwd OF E-EX-UNSUP throw ENDOF  fullsum-dot-bwd OF E-EX-UNSUP throw ENDOF
      pad-scatter OF E-EX-UNSUP throw ENDOF  scatter-add OF E-EX-UNSUP throw ENDOF
   ;MATCH ;

: EX-ROW-FWD ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ EX-REF-PTR {: xb:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd MIR-ROWS@ {: R:n :}  nd MIR-COLS@ {: C:n :}
   R 0 ?do  xb i C * T-AT   ob i C * T-AT   C   nd MIR-OP@   EX-ROW-FWD-1  loop ;

: EX-ROW-BWD-1 ( ptr a ptr a ptr a n opkind -- )
   MATCH opkind
      layernorm-bwd OF LN-BWD ENDOF  rmsnorm-bwd OF RMS-BWD ENDOF  softmax-row-bwd OF SM-BWD ENDOF
      add OF E-EX-UNSUP throw ENDOF  mul OF E-EX-UNSUP throw ENDOF
      scale OF E-EX-UNSUP throw ENDOF  bias OF E-EX-UNSUP throw ENDOF
      relu OF E-EX-UNSUP throw ENDOF  gelu OF E-EX-UNSUP throw ENDOF
      silu OF E-EX-UNSUP throw ENDOF  layernorm OF E-EX-UNSUP throw ENDOF
      rmsnorm OF E-EX-UNSUP throw ENDOF  softmax-row OF E-EX-UNSUP throw ENDOF
      matmul OF E-EX-UNSUP throw ENDOF  linear OF E-EX-UNSUP throw ENDOF
      residual-add OF E-EX-UNSUP throw ENDOF  cast OF E-EX-UNSUP throw ENDOF
      rope OF E-EX-UNSUP throw ENDOF  reshape OF E-EX-UNSUP throw ENDOF
      transpose OF E-EX-UNSUP throw ENDOF  slice OF E-EX-UNSUP throw ENDOF
      concat OF E-EX-UNSUP throw ENDOF  gather OF E-EX-UNSUP throw ENDOF
      relu-bwd OF E-EX-UNSUP throw ENDOF  gelu-bwd OF E-EX-UNSUP throw ENDOF
      silu-bwd OF E-EX-UNSUP throw ENDOF  rope-bwd OF E-EX-UNSUP throw ENDOF
      rowsum-bwd OF E-EX-UNSUP throw ENDOF  fullsum-dot-bwd OF E-EX-UNSUP throw ENDOF
      pad-scatter OF E-EX-UNSUP throw ENDOF  scatter-add OF E-EX-UNSUP throw ENDOF
   ;MATCH ;

\ p0 = cotangent row ; p1 = saved input (norms) or saved output (softmax) row.
: EX-ROW-BWD ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ EX-REF-PTR {: p0:ptr :}
   nd 1 MIR-IN@ EX-REF-PTR {: p1:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd MIR-ROWS@ {: R:n :}  nd MIR-COLS@ {: C:n :}
   R 0 ?do  p0 i C * T-AT   p1 i C * T-AT   ob i C * T-AT   C   nd MIR-OP@   EX-ROW-BWD-1  loop ;

\ ---- matmul / linear (inner dim = data-operand cols) ------------------------
: EX-MATMUL ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: xr:n :}  nd 1 MIR-IN@ {: wr:n :}
   xr EX-REF-PTR  wr EX-REF-PTR  nd EX-NODE-PTR
   nd MIR-ROWS@  xr EX-REF-COLS  nd MIR-COLS@  MATMUL ;

: EX-LINEAR ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: xr:n :}  nd 1 MIR-IN@ {: wr:n :}  nd 2 MIR-IN@ {: br:n :}
   xr EX-REF-PTR  wr EX-REF-PTR  br EX-REF-PTR  nd EX-NODE-PTR
   nd MIR-ROWS@  xr EX-REF-COLS  nd MIR-COLS@  LINEAR ;

\ ---- movement (attrs carry slice offsets; gather reads an int index scratch) -
: EX-BUILD-IDX ( n -- n ) {: r:n :}     \ float index operand -> EX-IDX ints; count
   r EX-REF-ELEMS {: n:n :}
   n EX-IDX-CAP > if E-EX-CAP throw then
   r EX-REF-PTR {: p:ptr :}
   n 0 ?do  p i T-GET 0.5 f+ f>s  EX-IDX i cells + !  loop
   n ;

: EX-RESHAPE ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: r:n :}
   r EX-REF-PTR  r EX-REF-ROWS  r EX-REF-COLS
   nd EX-NODE-PTR  nd MIR-ROWS@  nd MIR-COLS@  MOVE-RESHAPE ;

: EX-TRANSPOSE ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: r:n :}
   r EX-REF-PTR  r EX-REF-ROWS  r EX-REF-COLS
   nd EX-NODE-PTR  nd MIR-ROWS@  nd MIR-COLS@  MOVE-TRANSPOSE ;

: EX-SLICE ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: r:n :}  nd MIR-ATTR@ {: attr:n :}
   r EX-REF-PTR  r EX-REF-ROWS  r EX-REF-COLS
   attr MV-PA@  attr MV-PB@  nd EX-NODE-PTR  MOVE-SLICE ;

: EX-CONCAT ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: ra:n :}  nd 1 MIR-IN@ {: rb:n :}
   ra EX-REF-PTR  ra EX-REF-ROWS  ra EX-REF-COLS
   rb EX-REF-PTR  rb EX-REF-ROWS  rb EX-REF-COLS
   nd EX-NODE-PTR  MOVE-CONCAT ;

: EX-GATHER ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: rs:n :}  nd 1 MIR-IN@ {: rix:n :}
   rix EX-BUILD-IDX {: nix:n :}
   rs EX-REF-PTR  rs EX-REF-ROWS  rs EX-REF-COLS
   EX-IDX  nix  nd EX-NODE-PTR  MOVE-GATHER ;

\ ---- reduce / scatter backward references ----------------------------------
: EX-ROWSUM ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: r:n :}
   r EX-REF-PTR  r EX-REF-ROWS  r EX-REF-COLS
   nd EX-NODE-PTR  nd MIR-COLS@  ROWSUM-BWD ;

: EX-FULLSUM ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: rc:n :}  nd 1 MIR-IN@ {: rx:n :}
   rc EX-REF-PTR  rx EX-REF-PTR  rc EX-REF-ELEMS  nd EX-NODE-PTR  FULLSUM-DOT-BWD ;

: EX-PAD-SCATTER ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: r:n :}  nd MIR-ATTR@ {: attr:n :}
   r EX-REF-PTR  r EX-REF-ROWS  r EX-REF-COLS
   attr MV-PA@  nd MIR-ROWS@  nd EX-NODE-PTR  PAD-SCATTER ;

: EX-SCATTER-ADD ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ {: rc:n :}  nd 1 MIR-IN@ {: rix:n :}
   rix EX-BUILD-IDX drop
   rc EX-REF-PTR  rc EX-REF-ROWS  rc EX-REF-COLS
   EX-IDX  nd MIR-ROWS@  nd EX-NODE-PTR  SCATTER-ADD ;

\ ---- rope (adjacent column pairs; cos/sin at the pair's base column) --------
: EX-PAIR! ( r r ptr a n -- ) {: re:r im:r orow:ptr c0:n :}
   re orow c0 T-SET   im orow c0 1+ T-SET ;

: EX-ROPE-ROW ( ptr a ptr a ptr a ptr a n -- ) {: xr:ptr cr:ptr sr:ptr orow:ptr C:n :}
   C 2 / 0 ?do
      i 2 * {: c0:n :}
      xr c0 T-GET  xr c0 1+ T-GET  cr c0 T-GET  sr c0 T-GET  ROPE-PAIR
      orow c0  EX-PAIR!
   loop ;

: EX-ROPE-ROW-BWD ( ptr a ptr a ptr a ptr a n -- ) {: dz:ptr cr:ptr sr:ptr orow:ptr C:n :}
   C 2 / 0 ?do
      i 2 * {: c0:n :}
      dz c0 T-GET  dz c0 1+ T-GET  cr c0 T-GET  sr c0 T-GET  ROPE-BWD
      orow c0  EX-PAIR!
   loop ;

: EX-ROPE-FWD ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ EX-REF-PTR {: xp:ptr :}
   nd 1 MIR-IN@ EX-REF-PTR {: cp:ptr :}
   nd 2 MIR-IN@ EX-REF-PTR {: sp:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd MIR-ROWS@ {: R:n :}  nd MIR-COLS@ {: C:n :}
   R 0 ?do  xp i C * T-AT  cp i C * T-AT  sp i C * T-AT  ob i C * T-AT  C  EX-ROPE-ROW  loop ;

: EX-ROPE-BWD ( n -- ) {: nd:n :}
   nd 0 MIR-IN@ EX-REF-PTR {: dp:ptr :}
   nd 1 MIR-IN@ EX-REF-PTR {: cp:ptr :}
   nd 2 MIR-IN@ EX-REF-PTR {: sp:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd MIR-ROWS@ {: R:n :}  nd MIR-COLS@ {: C:n :}
   R 0 ?do  dp i C * T-AT  cp i C * T-AT  sp i C * T-AT  ob i C * T-AT  C  EX-ROPE-ROW-BWD  loop ;

\ ---- per-node dispatch (fail closed on a non-executable op) -----------------
: EX-NODE ( n -- ) {: nd:n :}
   nd MIR-OP@ MATCH opkind
      relu            OF nd EX-U            ENDOF
      gelu            OF nd EX-U            ENDOF
      silu            OF nd EX-U            ENDOF
      add             OF nd EX-EW2          ENDOF
      mul             OF nd EX-EW2          ENDOF
      scale           OF nd EX-EW2          ENDOF
      bias            OF nd EX-EW2          ENDOF
      residual-add    OF nd EX-EW2          ENDOF
      relu-bwd        OF nd EX-EW2          ENDOF
      gelu-bwd        OF nd EX-EW2          ENDOF
      silu-bwd        OF nd EX-EW2          ENDOF
      layernorm       OF nd EX-ROW-FWD      ENDOF
      rmsnorm         OF nd EX-ROW-FWD      ENDOF
      softmax-row     OF nd EX-ROW-FWD      ENDOF
      layernorm-bwd   OF nd EX-ROW-BWD      ENDOF
      rmsnorm-bwd     OF nd EX-ROW-BWD      ENDOF
      softmax-row-bwd OF nd EX-ROW-BWD      ENDOF
      matmul          OF nd EX-MATMUL       ENDOF
      linear          OF nd EX-LINEAR       ENDOF
      reshape         OF nd EX-RESHAPE      ENDOF
      transpose       OF nd EX-TRANSPOSE    ENDOF
      slice           OF nd EX-SLICE        ENDOF
      concat          OF nd EX-CONCAT       ENDOF
      gather          OF nd EX-GATHER       ENDOF
      rowsum-bwd      OF nd EX-ROWSUM       ENDOF
      fullsum-dot-bwd OF nd EX-FULLSUM      ENDOF
      pad-scatter     OF nd EX-PAD-SCATTER  ENDOF
      scatter-add     OF nd EX-SCATTER-ADD  ENDOF
      rope            OF nd EX-ROPE-FWD     ENDOF
      rope-bwd        OF nd EX-ROPE-BWD     ENDOF
      cast            OF E-EX-UNSUP throw   ENDOF
   ;MATCH ;

\ ---- buffer plan + execute over a node prefix ------------------------------
: EX-PLAN ( n -- ) {: n:n :}            \ assign each node an arena offset by shape
   0 EX-BUMP !
   n 0 ?do
      EX-BUMP @  i cells EX-OFF + !
      EX-BUMP @  i EX-NODE-ELEMS +  {: nb:n :}
      nb EX-ARENA-CELLS > if E-EX-CAP throw then
      nb EX-BUMP !
   loop ;

: EX-EXEC ( n -- )  0 ?do  i EX-NODE  loop ;

public

\ ---- membership: is an op-kind host-executable? (cast / decode are not) -----
\ the op-kind family makes the old OP-N range check unrepresentable; every op is
\ host-executable except cast (exhaustive MATCH predicate).
: EX-OP-OK? ( opkind -- bool )
   MATCH opkind
      cast OF false ENDOF
      add OF true ENDOF  mul OF true ENDOF  scale OF true ENDOF  bias OF true ENDOF
      relu OF true ENDOF  gelu OF true ENDOF  layernorm OF true ENDOF  rmsnorm OF true ENDOF
      softmax-row OF true ENDOF  matmul OF true ENDOF  linear OF true ENDOF
      residual-add OF true ENDOF  silu OF true ENDOF  rope OF true ENDOF
      reshape OF true ENDOF  transpose OF true ENDOF  slice OF true ENDOF  concat OF true ENDOF
      gather OF true ENDOF  relu-bwd OF true ENDOF  gelu-bwd OF true ENDOF  silu-bwd OF true ENDOF
      layernorm-bwd OF true ENDOF  rmsnorm-bwd OF true ENDOF  softmax-row-bwd OF true ENDOF
      rope-bwd OF true ENDOF  rowsum-bwd OF true ENDOF  fullsum-dot-bwd OF true ENDOF
      pad-scatter OF true ENDOF  scatter-add OF true ENDOF
   ;MATCH ;

\ ---- input binding + lifecycle ---------------------------------------------
: EX-RESET ( -- )  EX-IN-CAP 0 ?do  0 i cells EX-IN-SET + !  loop ;

: EX-BIND ( ptr a n -- ) {: p:ptr s:n :}   \ bind a model-input slot's host buffer
   s EX-IN-CK drop
   p  EX-IN-PTR s cells +  !
   1  EX-IN-SET s cells +  ! ;

\ ---- run a node prefix (forward slice) or the whole IR ---------------------
: EX-RUN-N ( n -- )  dup EX-PLAN  EX-EXEC ;
: EX-RUN ( -- )      MIR-N@ EX-RUN-N ;

\ ---- read a node's output buffer (after EX-RUN / EX-RUN-N) ------------------
: EX-OUT@ ( n -- ptr a ) {: nd:n :}
   nd 0 < nd MIR-N@ >= or if E-EX-NODE throw then
   nd EX-NODE-PTR ;

end-package
