\ maki/executor.f - the full-tensor host executor for the model IR (dot cad-7a).
\
\ Executes the captured model-IR node table (maki/model-ir.f) at TENSOR granularity
\ over host float-cell buffers - the GOLDEN composition oracle (docs/archive/cad-plan.md section 11)
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
\ table; node buffers are the executor's own arena. EX-RUN(-N) assigns one arena slot
\ per node (EX-PLAN); the checkpoint hooks EX-OFF!/EX-STEP let maki/checkpoint.f place
\ node buffers itself and execute nodes selectively. One concern: EXECUTION ONLY - no
\ planning, no gradients, no reporting. Fail closed: an op with no host reference
\ (cast / decode) is E-EX-UNSUP; an unbound input is E-EX-UNBOUND; arena / index
\ overflow is E-EX-CAP; a bad slot / node index is E-EX-SLOT / E-EX-NODE. Gather
\ indices are read and rounded directly by move.f; scatter alone converts its float
\ operand into EX-IDX integer scratch. maki -> habu; owns -5130..-5134.

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
require maki/segment.f
require maki/mha-op.f                 \ the MHA op-surface bridge: MHA-FWD/MHA-BWD + attr codec / geometry / recompute
require maki/spec.f
require maki/prec-attr.f              \ CPREC-PAYLOAD@: read the equation slot past the precision field
require maki/dropout.f                \ DO-APPLY / DO-PFIX / DO-ATTR-EVAL? / DO-NODE-SEED
require maki/swiglu.f                 \ SWIGLU-F: the fused silu(gate)*up scalar reference (EX-EW2)

\ ---- audited count projection (MIR typed item-count -> raw loop/compare cell) ---
\ Reopen the unsealed CAD-NUM package for one checked bridge over its private
\ ITEM-COUNT>N projection (no new TRUSTED), mirroring cad.f CAD-IX>N: a bounds
\ compare / count sink needs the raw cell the NODE-/SLOT-COUNT@ item-count cannot
\ flow into.
package CAD-NUM public
: EX-IC>N ( CAD-NUM:item-count -- n )  ITEM-COUNT>N ;
;package

-5130 constant E-EX-CAP       \ node arena / index scratch capacity exceeded
-5131 constant E-EX-UNSUP     \ op-kind has no host-executable reference (cast / decode)
-5132 constant E-EX-UNBOUND   \ an operand names a model-input slot with no bound buffer
-5133 constant E-EX-SLOT      \ model-input slot index out of range
-5134 constant E-EX-NODE      \ node index out of range (EX-OUT@ / EX-OFF! / EX-STEP)

package MAKI
private

\ Model-proportional table sizing (dot habu-size-model-proportional): the per-node offset table and
\ the per-slot input tables derive their size from the model at plan / reset time and grow past these
\ seeds, reusing the largest region across models (grow-to-largest). The old fixed caps become generous
\ sanity CEILINGS: an absurd model dies E-EX-CAP before any state change (transactional).
1024  constant EX-OFF-SEED    \ per-node offset table SEED (nodes); grows past this, ceiling = EX-ARENA-MAX
64    constant EX-IN-SEED     \ per-slot input table SEED (slots); grows past this
$8000 constant EX-IN-CEIL     \ generous input-slot sanity ceiling: a model past it dies E-EX-CAP
$8000  constant EX-ARENA-CELLS \ node-buffer arena SEED (float cells); the arena derives its
                               \ working size from the model at plan time and grows past this seed
$80000 constant EX-ARENA-MAX   \ generous sanity ceiling (float cells): a plan needing more is an
                               \ absurd model and dies E-EX-CAP before any node runs (transactional)
$400  constant EX-IDX-CAP     \ scatter index scratch (rows)

create EX-ARENA  EX-ARENA-CELLS cells allot   \ seed node-buffer pool (EX-ARENA-P starts here)
create EX-OFF    EX-OFF-SEED cells allot        \ per-node arena-offset table SEED (cells); EX-OFF-P starts here
variable EX-BUMP                                \ arena high-water (cells) during a plan
variable EX-ARENA-P                             \ current node-buffer base ptr (seed, or a grown region)
variable EX-ARENA-N                             \ current node-buffer capacity (float cells)
variable EX-OFF-P                               \ current offset-table base ptr (seed, or a grown region)
variable EX-OFF-CAP                             \ current offset-table capacity (nodes); grow-to-largest
variable EX-OFF-N                               \ live node count of the last bind (exact)
create EX-IN-PTR EX-IN-SEED cells allot         \ per-slot bound buffer pointer SEED; EX-IN-PP starts here
create EX-IN-SET EX-IN-SEED cells allot         \ per-slot bound flag SEED (0 = unbound); EX-IN-SP starts here
variable EX-IN-PP                               \ current ptr-table base ptr
variable EX-IN-SP                               \ current set-table base ptr
variable EX-IN-CAP                              \ current input-table capacity (slots); grow-to-largest
variable EX-IN-N                                \ live slot count of the last bind (exact)
create EX-IDX    EX-IDX-CAP cells allot          \ int index scratch (scatter)

\ The node-buffer arena, the per-node offset table, and the per-slot input tables are all model-
\ proportional (design dot habu-size-model-proportional): each derives its working size from the model
\ and grows past its seed, reusing the largest region across models (grow-to-largest; a grown region
\ abandons its predecessor, so the leak is bounded by the largest model, and the common case - every
\ model that fits the seed - never allots). The *-P base ptrs start at the seed regions.
EX-ARENA  EX-ARENA-P ! EX-ARENA-CELLS EX-ARENA-N !
EX-OFF    EX-OFF-P   ! EX-OFF-SEED    EX-OFF-CAP !
EX-IN-PTR EX-IN-PP   ! EX-IN-SET EX-IN-SP !  EX-IN-SEED EX-IN-CAP !

\ Grow a derived-size table to hold `n` items: reuse the current region when it already fits; else
\ allot a fresh larger region (grow-to-largest). A bind past the generous ceiling dies E-EX-CAP BEFORE
\ any allot or live-count store, so a failed bind leaves the prior table intact (transactional).
: EX-OFF-ENSURE ( n -- ) {: n:n :}              \ size the per-node offset table (bind at EX-PLAN / EX-OFF!)
   n EX-ARENA-MAX > if E-EX-CAP throw then       \ node count can't exceed arena cells: shares the absurd bound
   n EX-OFF-CAP @ > if
      here {: base:ptr :}  n cells allot  base EX-OFF-P !  n EX-OFF-CAP ! then
   n EX-OFF-N ! ;
: EX-IN-ENSURE ( n -- ) {: n:n :}               \ size the per-slot input + flag tables (bind at EX-RESET)
   n EX-IN-CEIL > if E-EX-CAP throw then
   n EX-IN-CAP @ > if
      here {: pb:ptr :}  n cells allot  pb EX-IN-PP !
      here {: sb:ptr :}  n cells allot  sb EX-IN-SP !
      n EX-IN-CAP ! then
   n EX-IN-N ! ;

\ ---- slot / node addressing ------------------------------------------------
: EX-IN-CK ( n -- n )                   \ validate a raw model-input slot index
   dup 0 < over SLOT-COUNT@ CAD-NUM:EX-IC>N >= or if E-EX-SLOT throw then ;

: EX-SLOT-PTR ( MIR:input-slot -- ptr r ) {: s:MIR:input-slot :}   \ bound buffer (fail closed)
   s SLOT>RAW EX-IN-CK {: raw:n :}
   EX-IN-SP @ raw T-AT @ 0= if E-EX-UNBOUND throw then
   EX-IN-PP @ raw T-AT @ ;

: EX-OFF@ ( CAD-KIND:node-id -- n )  NODE>RAW EX-OFF-P @ swap T-AT @ ;
: EX-NODE-PTR ( CAD-KIND:node-id -- ptr r )  EX-OFF@ {: off:n :}  EX-ARENA-P @ off T-AT ;

\ ---- operand-ref descriptor (input slot or producer node) -------------------
: EX-REF-PTR ( MIR:operand-ref -- ptr r ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT EX-SLOT-PTR else r MIR-REF-NODE EX-NODE-PTR then ;
: EX-REF-ROWS ( MIR:operand-ref -- CAD-KIND:rows ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-REF-NODE MIR-ROWS@ then ;
: EX-REF-COLS ( MIR:operand-ref -- CAD-KIND:cols ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-REF-NODE MIR-COLS@ then ;
: EX-REF-ELEMS ( MIR:operand-ref -- n ) {: r:MIR:operand-ref :}
   r EX-REF-ROWS  r EX-REF-COLS  SHAPE-ELEMS DIM-RAW ;
: EX-REF-NROWS ( MIR:operand-ref -- n )  EX-REF-ROWS ROWS-RAW ;
: EX-REF-NCOLS ( MIR:operand-ref -- n )  EX-REF-COLS COLS-RAW ;
: EX-NODE-NROWS ( CAD-KIND:node-id -- n )  MIR-ROWS@ ROWS-RAW ;
: EX-NODE-NCOLS ( CAD-KIND:node-id -- n )  MIR-COLS@ COLS-RAW ;

public
: EX-NODE-ELEMS ( CAD-KIND:node-id -- n ) {: nd:CAD-KIND:node-id :}
   nd MIR-ROWS@ nd MIR-COLS@ SHAPE-ELEMS DIM-RAW ;
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
      gelu-bwd2 OF E-EX-UNSUP throw ENDOF
      seg-attn OF E-EX-UNSUP throw ENDOF  seg-attn-bwd OF E-EX-UNSUP throw ENDOF
      equation OF E-EX-UNSUP throw ENDOF  bcast-mul OF E-EX-UNSUP throw ENDOF
      dropout OF E-EX-UNSUP throw ENDOF  dropout-bwd OF E-EX-UNSUP throw ENDOF
      swiglu OF E-EX-UNSUP throw ENDOF                 \ arity 2, not a unary scalar
      mha OF E-EX-UNSUP throw ENDOF  mha-bwd OF E-EX-UNSUP throw ENDOF
   ;MATCH ;

: EX-U ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: ap:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-ELEMS 0 ?do  ap i T-GET  nd MIR-OP@ EX-U-EL  ob i T-SET  loop ;

\ broadcast read b[r,c]: a 1-row operand reads row 0, a 1-col operand reads col 0.
: EX-BC@ ( ptr r n n n n -- r ) {: bp:ptr br:n bc:n r:n c:n :}
   br 1 = if 0 else r then {: rr:n :}
   bc 1 = if 0 else c then {: cc:n :}
   bp  rr bc *  cc +  T-GET ;

: EX-EW2-EL ( r r opkind -- r )
   MATCH opkind
      add OF ADD-F ENDOF  residual-add OF ADD-F ENDOF  bias OF ADD-F ENDOF
      mul OF MUL-F ENDOF  scale OF MUL-F ENDOF  bcast-mul OF MUL-F ENDOF
      swiglu OF SWIGLU-F ENDOF                          \ silu(gate)*up, both operands data
      relu-bwd OF RELU-BWD ENDOF  gelu-bwd OF GELU-BWD ENDOF  silu-bwd OF SILU-BWD ENDOF
      gelu-bwd2 OF GELU-BWD2 ENDOF
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
      seg-attn OF E-EX-UNSUP throw ENDOF  seg-attn-bwd OF E-EX-UNSUP throw ENDOF
      equation OF E-EX-UNSUP throw ENDOF
      dropout OF E-EX-UNSUP throw ENDOF  dropout-bwd OF E-EX-UNSUP throw ENDOF
      mha OF E-EX-UNSUP throw ENDOF  mha-bwd OF E-EX-UNSUP throw ENDOF
   ;MATCH ;

: EX-EW2 ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: ap:ptr :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: bref:MIR:operand-ref :}
   bref EX-REF-PTR {: bp:ptr :}  bref EX-REF-NROWS {: br:n :}  bref EX-REF-NCOLS {: bc:n :}
   nd EX-NODE-PTR {: ob:ptr :}  nd EX-NODE-NCOLS {: C:n :}
   nd EX-NODE-ELEMS 0 ?do
      ap i T-GET   bp br bc  i C /  i C mod  EX-BC@   nd MIR-OP@ EX-EW2-EL   ob i T-SET
   loop ;

\ ---- row words (layernorm / rmsnorm / softmax + VJPs) applied per row -------
: EX-ROW-FWD-1 ( ptr r ptr r n opkind -- )
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
      gelu-bwd2 OF E-EX-UNSUP throw ENDOF
      seg-attn OF E-EX-UNSUP throw ENDOF  seg-attn-bwd OF E-EX-UNSUP throw ENDOF
      equation OF E-EX-UNSUP throw ENDOF  bcast-mul OF E-EX-UNSUP throw ENDOF
      dropout OF E-EX-UNSUP throw ENDOF  dropout-bwd OF E-EX-UNSUP throw ENDOF
      swiglu OF E-EX-UNSUP throw ENDOF                 \ not a row op (elementwise)
      mha OF E-EX-UNSUP throw ENDOF  mha-bwd OF E-EX-UNSUP throw ENDOF
   ;MATCH ;

\ affine LayerNorm (arity 3): y = gamma*xhat + beta with gamma/beta (1xC) shared per row.
: EX-ROW-FWD-AFFINE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: xb:ptr :}
   nd 1 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: gb:ptr :}
   nd 2 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: bb:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-NROWS {: R:n :}  nd EX-NODE-NCOLS {: C:n :}
   R 0 ?do  xb i C * T-AT   ob i C * T-AT   gb  bb  C   LN-AFFINE-FWD  loop ;

: EX-ROW-FWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd LN-AFFINE? if  nd EX-ROW-FWD-AFFINE exit  then       \ explicit form, not the input count
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: xb:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-NROWS {: R:n :}  nd EX-NODE-NCOLS {: C:n :}
   R 0 ?do  xb i C * T-AT   ob i C * T-AT   C   nd MIR-OP@   EX-ROW-FWD-1  loop ;

: EX-ROW-BWD-1 ( ptr r ptr r ptr r n opkind -- )
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
      gelu-bwd2 OF E-EX-UNSUP throw ENDOF
      seg-attn OF E-EX-UNSUP throw ENDOF  seg-attn-bwd OF E-EX-UNSUP throw ENDOF
      equation OF E-EX-UNSUP throw ENDOF  bcast-mul OF E-EX-UNSUP throw ENDOF
      dropout OF E-EX-UNSUP throw ENDOF  dropout-bwd OF E-EX-UNSUP throw ENDOF
      swiglu OF E-EX-UNSUP throw ENDOF                 \ not a row op (elementwise)
      mha OF E-EX-UNSUP throw ENDOF  mha-bwd OF E-EX-UNSUP throw ENDOF
   ;MATCH ;

\ p0 = cotangent row ; p1 = saved input (norms) or saved output (softmax) row.
: EX-ROW-BWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: p0:ptr :}
   nd 1 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: p1:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-NROWS {: R:n :}  nd EX-NODE-NCOLS {: C:n :}
   R 0 ?do  p0 i C * T-AT   p1 i C * T-AT   ob i C * T-AT   C   nd MIR-OP@   EX-ROW-BWD-1  loop ;

\ ---- matmul / linear (inner dim = data-operand cols) ------------------------
: EX-MATMUL ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: xr:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: wr:MIR:operand-ref :}
   xr EX-REF-PTR  wr EX-REF-PTR  nd EX-NODE-PTR
   nd EX-NODE-NROWS  xr EX-REF-NCOLS  nd EX-NODE-NCOLS  MATMUL ;

: EX-LINEAR ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: xr:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: wr:MIR:operand-ref :}
   nd 2 MIR-INPUT-IDX MIR-IN@ {: br:MIR:operand-ref :}
   xr EX-REF-PTR  wr EX-REF-PTR  br EX-REF-PTR  nd EX-NODE-PTR
   nd EX-NODE-NROWS  xr EX-REF-NCOLS  nd EX-NODE-NCOLS  LINEAR ;

\ ---- movement (attrs carry slice offsets; gather reads float indices directly) -
: EX-BUILD-IDX ( MIR:operand-ref -- n ) {: r:MIR:operand-ref :}   \ float index operand -> EX-IDX ints; count
   r EX-REF-ELEMS {: n:n :}
   n EX-IDX-CAP > if E-EX-CAP throw then
   r EX-REF-PTR {: p:ptr :}
   n 0 ?do  p i T-GET 0.5 f+ f>s  EX-IDX i cells + !  loop
   n ;

: EX-RESHAPE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}
   r EX-REF-PTR  r EX-REF-NROWS  r EX-REF-NCOLS
   nd EX-NODE-PTR  nd EX-NODE-NROWS  nd EX-NODE-NCOLS  MOVE-RESHAPE ;

: EX-TRANSPOSE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}
   r EX-REF-PTR  r EX-REF-NROWS  r EX-REF-NCOLS
   nd EX-NODE-PTR  nd EX-NODE-NROWS  nd EX-NODE-NCOLS  MOVE-TRANSPOSE ;

: EX-SLICE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}  nd MIR-ATTR@ {: attr:n :}
   r EX-REF-PTR  r EX-REF-NROWS  r EX-REF-NCOLS
   attr MV-PA@  attr MV-PB@  nd EX-NODE-PTR  MOVE-SLICE ;

: EX-CONCAT ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: ra:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: rb:MIR:operand-ref :}
   ra EX-REF-PTR  ra EX-REF-NROWS  ra EX-REF-NCOLS
   rb EX-REF-PTR  rb EX-REF-NROWS  rb EX-REF-NCOLS
   nd EX-NODE-PTR  MOVE-CONCAT ;

: EX-GATHER ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: rs:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: rix:MIR:operand-ref :}
   rs EX-REF-PTR  rs EX-REF-NROWS  rs EX-REF-NCOLS
   rix EX-REF-PTR  rix EX-REF-ELEMS  nd EX-NODE-PTR  MOVE-GATHER ;

\ ---- reduce / scatter backward references ----------------------------------
: EX-ROWSUM ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}
   r EX-REF-PTR  r EX-REF-NROWS  r EX-REF-NCOLS
   nd EX-NODE-PTR  nd EX-NODE-NCOLS  ROWSUM-BWD ;

: EX-FULLSUM ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: rc:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: rx:MIR:operand-ref :}
   rc EX-REF-PTR  rx EX-REF-PTR  rc EX-REF-ELEMS  nd EX-NODE-PTR  FULLSUM-DOT-BWD ;

: EX-PAD-SCATTER ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}  nd MIR-ATTR@ {: attr:n :}
   r EX-REF-PTR  r EX-REF-NROWS  r EX-REF-NCOLS
   attr MV-PA@  nd EX-NODE-NROWS  nd EX-NODE-PTR  PAD-SCATTER ;

: EX-SCATTER-ADD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: rc:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: rix:MIR:operand-ref :}
   rix EX-BUILD-IDX drop
   rc EX-REF-PTR  rc EX-REF-NROWS  rc EX-REF-NCOLS
   EX-IDX  nd EX-NODE-NROWS  nd EX-NODE-PTR  SCATTER-ADD ;

\ ---- rope (adjacent column pairs; cos/sin at the pair's base column) --------
: EX-PAIR! ( r r ptr r n -- ) {: re:r im:r orow:ptr c0:n :}
   re orow c0 T-SET   im orow c0 1+ T-SET ;

: EX-ROPE-ROW ( ptr r ptr r ptr r ptr r n -- ) {: xr:ptr cr:ptr sr:ptr orow:ptr C:n :}
   C 2 / 0 ?do
      i 2 * {: c0:n :}
      xr c0 T-GET  xr c0 1+ T-GET  cr c0 T-GET  sr c0 T-GET  ROPE-PAIR
      orow c0  EX-PAIR!
   loop ;

: EX-ROPE-ROW-BWD ( ptr r ptr r ptr r ptr r n -- ) {: dz:ptr cr:ptr sr:ptr orow:ptr C:n :}
   C 2 / 0 ?do
      i 2 * {: c0:n :}
      dz c0 T-GET  dz c0 1+ T-GET  cr c0 T-GET  sr c0 T-GET  ROPE-BWD
      orow c0  EX-PAIR!
   loop ;

: EX-ROPE-FWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: xp:ptr :}
   nd 1 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: cp:ptr :}
   nd 2 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: sp:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-NROWS {: R:n :}  nd EX-NODE-NCOLS {: C:n :}
   R 0 ?do  xp i C * T-AT  cp i C * T-AT  sp i C * T-AT  ob i C * T-AT  C  EX-ROPE-ROW  loop ;

: EX-ROPE-BWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: dp:ptr :}
   nd 1 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: cp:ptr :}
   nd 2 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: sp:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-NROWS {: R:n :}  nd EX-NODE-NCOLS {: C:n :}
   R 0 ?do  dp i C * T-AT  cp i C * T-AT  sp i C * T-AT  ob i C * T-AT  C  EX-ROPE-ROW-BWD  loop ;

\ ---- segment causal attention (dot BTC-1): a fused per-block token-mixer -----
\ Forward reads Q,K,V (operands 0,1,2), block width T + causal flag from the attrs
\ cell, and writes O (B*T x d). rows = the node's output rows (B*T); d = Q's cols.
: EX-SEG-ATTN ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: qr:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: kr:MIR:operand-ref :}
   nd 2 MIR-INPUT-IDX MIR-IN@ {: vr:MIR:operand-ref :}
   nd MIR-ATTR@ {: attr:n :}
   qr EX-REF-PTR  kr EX-REF-PTR  vr EX-REF-PTR  nd EX-NODE-PTR
   nd EX-NODE-NROWS  attr SEG-T@  qr EX-REF-NCOLS  attr SEG-CAUSAL@  SEG-ATTN-FWD ;

\ Backward reads Q,K,V and the cotangent dO (operands 0,1,2,3) and writes the
\ combined [dQ;dK;dV] (3*B*T x d). rows = Q's rows (B*T), NOT the node's 3*B*T rows.
: EX-SEG-ATTN-BWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: qr:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: kr:MIR:operand-ref :}
   nd 2 MIR-INPUT-IDX MIR-IN@ {: vr:MIR:operand-ref :}
   nd 3 MIR-INPUT-IDX MIR-IN@ {: dor:MIR:operand-ref :}
   nd MIR-ATTR@ {: attr:n :}
   qr EX-REF-PTR  kr EX-REF-PTR  vr EX-REF-PTR  dor EX-REF-PTR  nd EX-NODE-PTR
   qr EX-REF-NROWS  attr SEG-T@  qr EX-REF-NCOLS  attr SEG-CAUSAL@  SEG-ATTN-BWD ;

\ ---- fused multi-head causal self-attention (dot habu-op-mha-fused) ---------
\ Forward reads X,Wqkv,bqkv,Wo,bo (operands 0..4) and the head count / sequence length /
\ causal flag from the attr, validates the whole (B,T,C,H,hd) geometry against the host
\ reference's fixed toy shape (fail closed E-MHA-GEOM - MHA-FWD reads module scratch sized
\ to those SPEC: constants), then writes Y (B*T x C). Extents come from the BIND (operand
\ shapes + attr), checked against - never sourced from - mha.f's constants.
: EX-MHA ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: xr:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: wqr:MIR:operand-ref :}
   nd 2 MIR-INPUT-IDX MIR-IN@ {: bqr:MIR:operand-ref :}
   nd 3 MIR-INPUT-IDX MIR-IN@ {: wor:MIR:operand-ref :}
   nd 4 MIR-INPUT-IDX MIR-IN@ {: bor:MIR:operand-ref :}
   nd MIR-ATTR@ {: attr:n :}
   xr EX-REF-NCOLS {: c:n :}
   xr EX-REF-NROWS c  attr MHA-H@  attr MHA-T@  attr MHA-CAUSAL@  MHA-GEOM-CK
   c  wqr EX-REF-NROWS wqr EX-REF-NCOLS  bqr EX-REF-NROWS bqr EX-REF-NCOLS
      wor EX-REF-NROWS wor EX-REF-NCOLS  bor EX-REF-NROWS bor EX-REF-NCOLS  MHA-PARAMS-CK
   xr EX-REF-PTR  wqr EX-REF-PTR  bqr EX-REF-PTR  wor EX-REF-PTR  bor EX-REF-PTR
   nd EX-NODE-PTR  MHA-FWD ;

\ Backward is SELF-CONTAINED: it reads all five forward inputs X,Wqkv,bqkv,Wo,bo (operands
\ 0..4) and the cotangent dY (operand 5), validates the whole geometry, RE-RUNS MHA-FWD from
\ THIS node's own inputs to rebuild the module-private tape (so a two-MHA model differentiates
\ each node from its OWN forward, not whichever ran last), then writes the combined
\ [dX|dWqkv|dbqkv|dWo|dbo] gradient buffer (TOTAL x 1) by handing MHA-BWD five sub-pointers at
\ the mha-op.f cumulative offsets. The reverse transform (maki/backward.f) slices the same
\ offsets back into one gradient per input.
: EX-MHA-BWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: xr:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: wqr:MIR:operand-ref :}
   nd 2 MIR-INPUT-IDX MIR-IN@ {: bqr:MIR:operand-ref :}
   nd 3 MIR-INPUT-IDX MIR-IN@ {: wor:MIR:operand-ref :}
   nd 4 MIR-INPUT-IDX MIR-IN@ {: bor:MIR:operand-ref :}
   nd 5 MIR-INPUT-IDX MIR-IN@ {: dor:MIR:operand-ref :}
   nd MIR-ATTR@ {: attr:n :}
   xr EX-REF-NROWS {: bt:n :}  xr EX-REF-NCOLS {: c:n :}
   bt c  attr MHA-H@  attr MHA-T@  attr MHA-CAUSAL@  MHA-GEOM-CK
   c  wqr EX-REF-NROWS wqr EX-REF-NCOLS  bqr EX-REF-NROWS bqr EX-REF-NCOLS
      wor EX-REF-NROWS wor EX-REF-NCOLS  bor EX-REF-NROWS bor EX-REF-NCOLS  MHA-PARAMS-CK
   dor EX-REF-NROWS dor EX-REF-NCOLS  bt c MHA-DY-CK          \ dY (B*T, C)
   xr EX-REF-PTR  wqr EX-REF-PTR  bqr EX-REF-PTR  wor EX-REF-PTR  bor EX-REF-PTR  MHA-BWD-RECOMPUTE
   nd EX-NODE-PTR {: gb:ptr :}
   xr EX-REF-PTR  wqr EX-REF-PTR  wor EX-REF-PTR  dor EX-REF-PTR
   gb bt c 0 MHA-BWD-OFF T-AT   gb bt c 1 MHA-BWD-OFF T-AT   gb bt c 2 MHA-BWD-OFF T-AT
   gb bt c 3 MHA-BWD-OFF T-AT   gb bt c 4 MHA-BWD-OFF T-AT
   MHA-BWD ;

\ ---- equation (docs/model-unified.md stage 1) ------------------------------
\ A SPEC:-declared einsum node: its attrs cell's LOW payload is the equation's spec-
\ registry slot (maki/spec.f), the HIGH field its compute precision (maki/prec-attr.f);
\ CPREC-PAYLOAD@ recovers the slot past that field. The precision selects the device MMA
\ dtype at lowering only - the host executor is f32/f64 EXACT and ignores it. The executor
\ writes each operand buffer and the output buffer into the registry transfer cells, then
\ runs the equation's generated host RUN word (EQ-EXEC), which binds those buffers to the
\ equation's tensors and executes the checked kernel. Operand order matches factor order.
: EX-EQUATION ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-ATTR@ CPREC-PAYLOAD@ >EQ-SLOT {: s:eq-slot :}
   s EQ-K@ 0 ?do
      nd i MIR-INPUT-IDX MIR-IN@ EX-REF-PTR  i EQ-ARG-SET!
   loop
   nd EX-NODE-PTR EQ-OUT-SET!
   s EQ-EXEC ;

\ ---- dropout (dot habu-dropout-op-train): masked 1/(1-p) scale over one buffer ------
\ Forward reads x (operand 0); the mask stream is seeded from THIS node's table index +
\ the run seed (DO-NODE-SEED), so a run is bit-reproducible and the VJP redraws the same
\ mask. mode/pfix decode from the attr. eval mode copies (identity).
: EX-DROPOUT ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: xb:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd MIR-ATTR@ {: attr:n :}
   xb ob nd EX-NODE-ELEMS  attr DO-PFIX  attr DO-ATTR-EVAL?  nd NODE>RAW DO-NODE-SEED  DO-APPLY ;

\ Backward reads dy (operand 0) and the FORWARD node (operand 1) - only for ITS table
\ index, which reseeds the identical mask stream. Same mode/pfix attr, so dropped elements
\ get exactly zero gradient and eval passes dy through unchanged.
: EX-DROPOUT-BWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: cb:ptr :}
   nd 1 MIR-INPUT-IDX MIR-IN@ MIR-REF-NODE NODE>RAW {: fwdraw:n :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd MIR-ATTR@ {: attr:n :}
   cb ob nd EX-NODE-ELEMS  attr DO-PFIX  attr DO-ATTR-EVAL?  fwdraw DO-NODE-SEED  DO-APPLY ;

\ ---- per-node dispatch (fail closed on a non-executable op) -----------------
: EX-NODE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-OP@ MATCH opkind
      relu            OF nd EX-U            ENDOF
      gelu            OF nd EX-U            ENDOF
      silu            OF nd EX-U            ENDOF
      add             OF nd EX-EW2          ENDOF
      mul             OF nd EX-EW2          ENDOF
      scale           OF nd EX-EW2          ENDOF
      bias            OF nd EX-EW2          ENDOF
      bcast-mul       OF nd EX-EW2          ENDOF
      residual-add    OF nd EX-EW2          ENDOF
      relu-bwd        OF nd EX-EW2          ENDOF
      gelu-bwd        OF nd EX-EW2          ENDOF
      gelu-bwd2       OF nd EX-EW2          ENDOF
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
      seg-attn        OF nd EX-SEG-ATTN     ENDOF
      seg-attn-bwd    OF nd EX-SEG-ATTN-BWD ENDOF
      mha             OF nd EX-MHA          ENDOF
      mha-bwd         OF nd EX-MHA-BWD      ENDOF
      cast            OF E-EX-UNSUP throw   ENDOF
      equation        OF nd EX-EQUATION     ENDOF
      dropout         OF nd EX-DROPOUT      ENDOF
      dropout-bwd     OF nd EX-DROPOUT-BWD  ENDOF
      swiglu          OF nd EX-EW2          ENDOF
   ;MATCH ;

\ ---- buffer plan + execute over a node prefix ------------------------------
\ grow the node-buffer arena to hold `need` float cells (derived from the model's node shapes):
\ reuse the current region when it already fits; else allot a fresh larger region (grow-to-largest,
\ abandoning the predecessor - leak bounded by the largest model). A need past the generous ceiling
\ is an absurd model and dies NAMED here, before any node executes, so a failed plan leaves the
\ prior arena (and the prior model's outputs) intact - the transactional boundary.
: EX-ARENA-ENSURE ( n -- ) {: need:n :}
   need EX-ARENA-N @ <= if exit then
   need EX-ARENA-MAX > if E-EX-CAP throw then
   here {: base:ptr :}
   need cells allot
   base EX-ARENA-P !
   need EX-ARENA-N ! ;

: EX-PLAN ( n -- ) {: n:n :}            \ size the offset table + arena, assign each node an arena offset by shape
   n EX-OFF-ENSURE
   0 EX-BUMP !
   n 0 ?do
      EX-BUMP @  EX-OFF-P @ i T-AT !
      EX-BUMP @  i MIR-NODE-ID EX-NODE-ELEMS +  EX-BUMP !
   loop
   EX-BUMP @ EX-ARENA-ENSURE ;

: EX-EXEC ( n -- )  0 ?do  i MIR-NODE-ID EX-NODE  loop ;

public

\ ---- membership: is an op-kind host-executable? (cast / decode are not) -----
\ the op-kind family makes the old OP-N range check unrepresentable; every op is
\ host-executable except cast (exhaustive MATCH predicate). equation dispatches to its
\ generated host RUN word (EX-EQUATION, docs/model-unified.md stage 1), so it IS host-
\ executable; only cast (a non-executable decode) stays not host-executable.
: EX-OP-OK? ( opkind -- bool )
   MATCH opkind
      cast OF false ENDOF
      equation OF true ENDOF
      add OF true ENDOF  mul OF true ENDOF  scale OF true ENDOF  bias OF true ENDOF
      bcast-mul OF true ENDOF
      relu OF true ENDOF  gelu OF true ENDOF  layernorm OF true ENDOF  rmsnorm OF true ENDOF
      softmax-row OF true ENDOF  matmul OF true ENDOF  linear OF true ENDOF
      residual-add OF true ENDOF  silu OF true ENDOF  rope OF true ENDOF
      reshape OF true ENDOF  transpose OF true ENDOF  slice OF true ENDOF  concat OF true ENDOF
      gather OF true ENDOF  relu-bwd OF true ENDOF  gelu-bwd OF true ENDOF  silu-bwd OF true ENDOF
      layernorm-bwd OF true ENDOF  rmsnorm-bwd OF true ENDOF  softmax-row-bwd OF true ENDOF
      rope-bwd OF true ENDOF  rowsum-bwd OF true ENDOF  fullsum-dot-bwd OF true ENDOF
      pad-scatter OF true ENDOF  scatter-add OF true ENDOF  gelu-bwd2 OF true ENDOF
      seg-attn OF true ENDOF  seg-attn-bwd OF true ENDOF
      dropout OF true ENDOF  dropout-bwd OF true ENDOF
      swiglu OF true ENDOF
      mha OF true ENDOF  mha-bwd OF true ENDOF
   ;MATCH ;

\ ---- input binding + lifecycle ---------------------------------------------
: EX-RESET ( -- )                       \ size the input tables to the model's slot count, clear the flags
   SLOT-COUNT@ CAD-NUM:EX-IC>N {: n:n :}  n EX-IN-ENSURE
   n 0 ?do  0 EX-IN-SP @ i T-AT !  loop ;

: EX-BIND ( ptr r MIR:input-slot -- ) {: p:ptr s:MIR:input-slot :}   \ bind a model-input slot's host buffer
   s SLOT>RAW EX-IN-CK {: raw:n :}
   p  EX-IN-PP @ raw T-AT  !
   1  EX-IN-SP @ raw T-AT  ! ;

\ ---- run a node prefix (forward slice) or the whole IR ---------------------
: EX-RUN-N ( n -- )  dup EX-PLAN  EX-EXEC ;
: EX-RUN ( -- )      NODE-COUNT@ CAD-NUM:EX-IC>N EX-RUN-N ;

\ plan node buffers only (no execution): the async-DAG replay driver
\ (maki/plan-ir.f) places the arena once, then EX-STEPs nodes in the sealed
\ deterministic replay order instead of EX-EXEC's hidden index order.
: EX-PLAN-N ( n -- )  EX-PLAN ;

\ ---- external buffer plan + single-node execution (the checkpoint hooks) ----
\ maki/checkpoint.f places node buffers itself (persistent boundaries + one
\ shared interior scratch window; docs/maki/train.md "Gradient checkpointing")
\ and runs nodes selectively. EX-OFF! validates the node index and the buffer
\ end against the arena; EX-STEP validates the index and executes one node
\ under the CURRENT offsets. EX-RUN(-N) overwrites the offsets (EX-PLAN), so a
\ custom plan is valid only until the next EX-RUN(-N).
: EX-OFF! ( n CAD-KIND:node-id -- ) {: off:n nd:CAD-KIND:node-id :}   \ arena offset (cells), node
   nd NODE>RAW {: raw:n :}
   raw 0 < raw NODE-COUNT@ CAD-NUM:EX-IC>N >= or if E-EX-NODE throw then
   off 0 <  off nd EX-NODE-ELEMS + EX-ARENA-N @ >  or if E-EX-CAP throw then   \ validate off before any grow
   NODE-COUNT@ CAD-NUM:EX-IC>N EX-OFF-ENSURE                                   \ checkpoint path sizes the table (no EX-PLAN)
   off EX-OFF-P @ raw T-AT ! ;

: EX-STEP ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}   \ execute one node under the current plan
   nd NODE>RAW {: raw:n :}
   raw 0 < raw NODE-COUNT@ CAD-NUM:EX-IC>N >= or if E-EX-NODE throw then
   nd EX-NODE ;

\ ---- read a node's output buffer (after EX-RUN / EX-RUN-N) ------------------
: EX-OUT@ ( CAD-KIND:node-id -- ptr r ) {: nd:CAD-KIND:node-id :}
   nd NODE>RAW {: raw:n :}
   raw 0 < raw NODE-COUNT@ CAD-NUM:EX-IC>N >= or if E-EX-NODE throw then
   nd EX-NODE-PTR ;

;package
