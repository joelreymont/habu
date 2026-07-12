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
: EX-IN-CK ( n -- n )                   \ validate a raw model-input slot index
   dup 0 < over MIR-IN-SLOTS@ >= or if E-EX-SLOT throw then ;

: EX-SLOT-PTR ( MIR:input-slot -- ptr a ) {: s:MIR:input-slot :}
   s SLOT>RAW EX-IN-CK {: raw:n :}
   raw cells EX-IN-SET + @ 0= if E-EX-UNBOUND throw then
   EX-IN-PTR raw cells + @ ;

: EX-OFF@ ( CAD-KIND:node-id -- n )  NODE>RAW cells EX-OFF + @ ;
: EX-NODE-PTR ( CAD-KIND:node-id -- ptr a )  EX-OFF@ {: off:n :}  EX-ARENA off T-AT ;

\ ---- operand-ref descriptor (input slot or producer node) -------------------
: EX-REF-PTR ( MIR:operand-ref -- ptr a ) {: r:MIR:operand-ref :}
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
: EX-U-EL ( r n -- r ) {: x:r op:n :}
   op case
      OP-RELU of x RELU-F endof
      OP-GELU of x GELU-F endof
      OP-SILU of x SILU-F endof
      E-EX-UNSUP throw
   endcase ;

: EX-U ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-OP@ {: op:n :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: ap:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-ELEMS 0 ?do  ap i T-GET op EX-U-EL  ob i T-SET  loop ;

\ broadcast read b[r,c]: a 1-row operand reads row 0, a 1-col operand reads col 0.
: EX-BC@ ( ptr a n n n n -- r ) {: bp:ptr br:n bc:n r:n c:n :}
   br 1 = if 0 else r then {: rr:n :}
   bc 1 = if 0 else c then {: cc:n :}
   bp  rr bc *  cc +  T-GET ;

: EX-EW2-EL ( r r n -- r ) {: a:r b:r op:n :}
   op case
      OP-ADD          of a b ADD-F   endof
      OP-RESIDUAL-ADD of a b ADD-F   endof
      OP-BIAS         of a b ADD-F   endof
      OP-MUL          of a b MUL-F   endof
      OP-SCALE        of a b MUL-F   endof
      OP-RELU-BWD     of a b RELU-BWD endof
      OP-GELU-BWD     of a b GELU-BWD endof
      OP-SILU-BWD     of a b SILU-BWD endof
      E-EX-UNSUP throw
   endcase ;

: EX-EW2 ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-OP@ {: op:n :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: ap:ptr :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: bref:MIR:operand-ref :}
   bref EX-REF-PTR {: bp:ptr :}  bref EX-REF-NROWS {: br:n :}  bref EX-REF-NCOLS {: bc:n :}
   nd EX-NODE-PTR {: ob:ptr :}  nd EX-NODE-NCOLS {: C:n :}
   nd EX-NODE-ELEMS 0 ?do
      ap i T-GET   bp br bc  i C /  i C mod  EX-BC@   op EX-EW2-EL   ob i T-SET
   loop ;

\ ---- row words (layernorm / rmsnorm / softmax + VJPs) applied per row -------
: EX-ROW-FWD-1 ( ptr a ptr a n n -- ) {: xb:ptr ob:ptr n:n op:n :}
   op case
      OP-LAYERNORM   of xb ob n LN-FWD  endof
      OP-RMSNORM     of xb ob n RMS-FWD endof
      OP-SOFTMAX-ROW of xb ob n SM-FWD  endof
      E-EX-UNSUP throw
   endcase ;

: EX-ROW-FWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: xb:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-NROWS {: R:n :}  nd EX-NODE-NCOLS {: C:n :}  nd MIR-OP@ {: op:n :}
   R 0 ?do  xb i C * T-AT   ob i C * T-AT   C   op   EX-ROW-FWD-1  loop ;

: EX-ROW-BWD-1 ( ptr a ptr a ptr a n n -- ) {: p0:ptr p1:ptr ob:ptr n:n op:n :}
   op case
      OP-LAYERNORM-BWD   of p0 p1 ob n LN-BWD  endof
      OP-RMSNORM-BWD     of p0 p1 ob n RMS-BWD endof
      OP-SOFTMAX-ROW-BWD of p0 p1 ob n SM-BWD  endof
      E-EX-UNSUP throw
   endcase ;

\ p0 = cotangent row ; p1 = saved input (norms) or saved output (softmax) row.
: EX-ROW-BWD ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: p0:ptr :}
   nd 1 MIR-INPUT-IDX MIR-IN@ EX-REF-PTR {: p1:ptr :}
   nd EX-NODE-PTR {: ob:ptr :}
   nd EX-NODE-NROWS {: R:n :}  nd EX-NODE-NCOLS {: C:n :}  nd MIR-OP@ {: op:n :}
   R 0 ?do  p0 i C * T-AT   p1 i C * T-AT   ob i C * T-AT   C   op   EX-ROW-BWD-1  loop ;

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

\ ---- movement (attrs carry slice offsets; gather reads an int index scratch) -
: EX-BUILD-IDX ( MIR:operand-ref -- n ) {: r:MIR:operand-ref :}
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
   rix EX-BUILD-IDX {: nix:n :}
   rs EX-REF-PTR  rs EX-REF-NROWS  rs EX-REF-NCOLS
   EX-IDX  nix  nd EX-NODE-PTR  MOVE-GATHER ;

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

\ ---- per-node dispatch (fail closed on a non-executable op) -----------------
: EX-NODE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-OP@ case
      OP-RELU            of nd EX-U            endof
      OP-GELU            of nd EX-U            endof
      OP-SILU            of nd EX-U            endof
      OP-ADD             of nd EX-EW2          endof
      OP-MUL             of nd EX-EW2          endof
      OP-SCALE           of nd EX-EW2          endof
      OP-BIAS            of nd EX-EW2          endof
      OP-RESIDUAL-ADD    of nd EX-EW2          endof
      OP-RELU-BWD        of nd EX-EW2          endof
      OP-GELU-BWD        of nd EX-EW2          endof
      OP-SILU-BWD        of nd EX-EW2          endof
      OP-LAYERNORM       of nd EX-ROW-FWD      endof
      OP-RMSNORM         of nd EX-ROW-FWD      endof
      OP-SOFTMAX-ROW     of nd EX-ROW-FWD      endof
      OP-LAYERNORM-BWD   of nd EX-ROW-BWD      endof
      OP-RMSNORM-BWD     of nd EX-ROW-BWD      endof
      OP-SOFTMAX-ROW-BWD of nd EX-ROW-BWD      endof
      OP-MATMUL          of nd EX-MATMUL       endof
      OP-LINEAR          of nd EX-LINEAR       endof
      OP-RESHAPE         of nd EX-RESHAPE      endof
      OP-TRANSPOSE       of nd EX-TRANSPOSE    endof
      OP-SLICE           of nd EX-SLICE        endof
      OP-CONCAT          of nd EX-CONCAT       endof
      OP-GATHER          of nd EX-GATHER       endof
      OP-ROWSUM-BWD      of nd EX-ROWSUM       endof
      OP-FULLSUM-DOT-BWD of nd EX-FULLSUM      endof
      OP-PAD-SCATTER     of nd EX-PAD-SCATTER  endof
      OP-SCATTER-ADD     of nd EX-SCATTER-ADD  endof
      OP-ROPE            of nd EX-ROPE-FWD     endof
      OP-ROPE-BWD        of nd EX-ROPE-BWD     endof
      E-EX-UNSUP throw
   endcase ;

\ ---- buffer plan + execute over a node prefix ------------------------------
: EX-PLAN ( n -- ) {: n:n :}            \ assign each node an arena offset by shape
   0 EX-BUMP !
   n 0 ?do
      EX-BUMP @  i cells EX-OFF + !
      EX-BUMP @  i MIR-NODE-ID EX-NODE-ELEMS +  {: nb:n :}
      nb EX-ARENA-CELLS > if E-EX-CAP throw then
      nb EX-BUMP !
   loop ;

: EX-EXEC ( n -- )  0 ?do  i MIR-NODE-ID EX-NODE  loop ;

public

\ ---- membership: is an op-kind host-executable? (cast / decode are not) -----
: EX-OP-OK? ( n -- bool ) {: op:n :}
   op 0 < 0=  op OP-N <  and  op OP-CAST <>  and ;

\ ---- input binding + lifecycle ---------------------------------------------
: EX-RESET ( -- )  EX-IN-CAP 0 ?do  0 i cells EX-IN-SET + !  loop ;

: EX-BIND ( ptr a MIR:input-slot -- ) {: p:ptr s:MIR:input-slot :}
   s SLOT>RAW EX-IN-CK {: raw:n :}
   p  EX-IN-PTR raw cells +  !
   1  EX-IN-SET raw cells +  ! ;

\ ---- run a node prefix (forward slice) or the whole IR ---------------------
: EX-RUN-N ( n -- )  dup EX-PLAN  EX-EXEC ;
: EX-RUN ( -- )      MIR-N@ EX-RUN-N ;

\ ---- read a node's output buffer (after EX-RUN / EX-RUN-N) ------------------
: EX-OUT@ ( CAD-KIND:node-id -- ptr a ) {: nd:CAD-KIND:node-id :}
   nd NODE>RAW {: raw:n :}
   raw 0 < raw MIR-N@ >= or if E-EX-NODE throw then
   nd EX-NODE-PTR ;

end-package
