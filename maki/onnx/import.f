\ maki/onnx/import.f - ONNX graph builder: parsed tables -> runnable model IR.
\
\ The top of the ONNX importer (dot habu-maki-onnx-graph): ONNX:IMPORT parses a
\ serialized ModelProto (maki/onnx/graph.f walkers) and builds the SAME model-IR
\ node table the MODEL: capture produces (maki/model-ir.f), so the fusion
\ planner (FP-BUILD) and the host executor (EX-RUN) run the imported graph
\ unchanged. Graph inputs and initializers become MIR input slots (f32,
\ row-major); initializer payloads are materialized into a float-cell arena and
\ ONNX:BIND-INITS binds them to the executor; nodes are walked in NodeProto
\ order - the spec says an ONNX graph is topologically sorted, and that
\ assumption is VALIDATED: every node input must already be bound (initializer,
\ graph input, or earlier node output) or E-ONNX-TOPO throws.
\
\ Op coverage IS the ONNX:LOWER table (maki/onnx.f): every node passes through
\ it first, so an unsupported op gets the table's existing E-MK-ONNX rejection.
\ The IR mapping on top of that coverage: Add->OP-ADD (same shape) / OP-BIAS
\ (1xC second operand, row broadcast), Mul->OP-MUL (same shape) / OP-SCALE (1x1
\ second operand, scalar broadcast) - ONNX numpy broadcast mapped onto the
\ capture-legal classes (maki/bcast.f BC-CLASS, the same mapping EX-BC@ /
\ SHP-LEGAL? use); any other numpy shape fails closed E-ONNX-SHAPE.
\ Relu->OP-RELU, Softmax->OP-SOFTMAX-ROW (axis must be the last: -1 or 1),
\ Gemm: the default affine form (alpha=beta=1, transA=transB=0) is the single-node
\ OP-MATMUL (2 inputs) / OP-LINEAR (3 inputs, bias 1xN); attributes compose into a
\ node chain otherwise - transA/transB insert a TRANSPOSE on that operand, a non-unit
\ alpha/beta inserts an OP-SCALE against a synthesized 1x1 constant, beta=0 drops C,
\ beta=1 adds C as a bias; any non-Gemm attribute rejects E-ONNX-ATTR. Movement
\ ops (ONNX:MOVE-KIND coverage): Reshape->OP-RESHAPE (target [R,C] read from the
\ INT64 shape initializer; a 0 dim copies the input dim, one -1 dim is inferred from
\ the element count, a runtime-computed shape fails E-ONNX-DYNSHAPE),
\ Transpose->OP-TRANSPOSE (2D; perm absent or exactly [1,0], else E-ONNX-ATTR),
\ Concat->OP-CONCAT (2 inputs, axis 0 row-append), Slice->OP-SLICE (starts/ends INT64
\ operands, axis 0 / unit step, ONNX index clamp, empty range rejected), Gather->
\ OP-GATHER (axis 0; the INT64 indices operand is resolved against the data rows -
\ negatives add rows, out-of-range rejects - then materialized into the float arena so
\ the executor's float-index read runs unchanged) - each carries MV-PACK'd attrs and
\ host-executes on the maki/move.f
\ references. v1 output contract: exactly one graph output, the LAST node
\ (E-ONNX-OUTPUT), with the declared output shape checked (E-ONNX-SHAPE).
\
\ Fail closed: unresolved input E-ONNX-TOPO; rebound output name (SSA)
\ E-ONNX-NAME; wrong input count E-ONNX-ARITY; arena overflow E-ONNX-CAP;
\ accessor index E-ONNX-IDX (codes in maki/onnx/graph.f). maki -> habu only.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/memory.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require maki/onnx.f
require maki/onnx/proto.f
require maki/onnx/graph.f
require maki/op-kind.f
require maki/bcast.f
require maki/tensor.f
require maki/tensor-value.f
require maki/array.f
require maki/model-ir.f
require maki/executor.f

package ONNX

\ ---- name binding table (name slot -> MIR operand ref) ----------------------
create IMP-REF OGN-CAP cells allot             \ MIR ref (input slot ref or node index)
create IMP-SET OGN-CAP cells allot             \ 1 = bound
variable IMP-IN-N                              \ runtime (non-initializer) input count
create IMP-INSLOT OGIN-CAP cells allot         \ runtime input ordinal -> MIR slot
create IMP-INGI   OGIN-CAP cells allot         \ runtime input ordinal -> graph-input index

\ ---- initializer materialization arena (float cells, the executor's dtype) --
$4000 constant IMP-ARENA-CELLS
create IMP-ARENA IMP-ARENA-CELLS cells allot
create IMP-SLOT OGI-CAP cells allot            \ initializer -> MIR input slot
create IMP-AOFF OGI-CAP cells allot            \ initializer -> arena offset (cells)
variable IMP-BUMP

\ ---- synthetic scalar constants (Gemm alpha/beta): 1x1 f32 slots into the arena ----
32 constant SYN-CAP
create SYN-SLOT SYN-CAP cells allot            \ synthetic constant -> MIR input slot
create SYN-OFF  SYN-CAP cells allot            \ synthetic constant -> arena offset (cells)
variable SYN-N

: IMP-RESET ( -- )
   0 IMP-BUMP !  0 IMP-IN-N !  0 SYN-N !
   OGN-CAP 0 ?do  0 IMP-SET i cells + !  loop ;

: IMP-BIND ( n n -- ) {: ni:n ref:n :}         \ bind a name slot to a MIR ref (SSA)
   IMP-SET ni cells + @ 0<> if E-ONNX-NAME throw then
   ref IMP-REF ni cells + !
   1 IMP-SET ni cells + ! ;

: IMP-RESOLVE ( n -- n ) {: ni:n :}            \ name slot -> MIR ref; unbound fails closed
   IMP-SET ni cells + @ 0= if E-ONNX-TOPO throw then
   IMP-REF ni cells + @ ;

\ ---- operand-ref shape facts (input slot or producer node) ------------------
: IMP-REF-ROWS ( n -- n ) {: r:n :}
   r MAKI:MIR-REF-INPUT? if r MAKI:MIR-REF-SLOT MAKI:MIR-SLOT-ROWS@ else r MAKI:MIR-ROWS@ then ;
: IMP-REF-COLS ( n -- n ) {: r:n :}
   r MAKI:MIR-REF-INPUT? if r MAKI:MIR-REF-SLOT MAKI:MIR-SLOT-COLS@ else r MAKI:MIR-COLS@ then ;

\ ---- inputs: graph inputs first (initializer-listed ones defer), then initializers --
: IMP-INIT-FIND ( n -- n bool ) {: ni:n :}     \ name slot -> initializer index?
   OGI# 0 ?do
      i OGI-NAME@ ni = if i true unloop exit then
   loop 0 false ;

: IMP-INPUT-1 ( n -- ) {: gi:n :}
   gi OGIN-NAME@ {: ni:n :}
   ni IMP-INIT-FIND if {: iz:n :}              \ input listed as initializer (legacy export):
      gi OGIN-ROWS@ iz OGI-ROWS@ <>  gi OGIN-COLS@ iz OGI-COLS@ <>  or
         if E-ONNX-SHAPE throw then            \ shapes must agree; the initializer binds it
      exit then
   drop
   gi OGIN-ROWS@ gi OGIN-COLS@ MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI:MIR-INPUT+ {: s:n :}
   ni s MAKI:MIR-IN-REF IMP-BIND
   s  IMP-IN-N @ cells IMP-INSLOT + !
   gi IMP-IN-N @ cells IMP-INGI + !
   IMP-IN-N @ 1+ IMP-IN-N ! ;

: IMP-INIT-1 ( n -- ) {: iz:n :}
   iz OGI-ROWS@ {: rows:n :}  iz OGI-COLS@ {: cols:n :}
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI:MIR-INPUT+ {: s:n :}
   iz OGI-NAME@ s MAKI:MIR-IN-REF IMP-BIND
   s iz cells IMP-SLOT + !
   rows cols * {: e:n :}
   IMP-BUMP @ e + IMP-ARENA-CELLS > if E-ONNX-CAP throw then
   IMP-BUMP @ iz cells IMP-AOFF + !
   IMP-BUMP @ e + IMP-BUMP ! ;

\ decode one initializer's LE f32 payload into its arena buffer
: IMP-MAT-1 ( ptr u8 n -- ) {: a:ptr iz:n :}
   iz OGI-OFF@ {: off:n :}
   iz OGI-ROWS@ iz OGI-COLS@ * {: e:n :}
   IMP-ARENA  iz cells IMP-AOFF + @  T-AT {: dst:ptr :}
   e 0 ?do
      a off i 4 * + +  SF-LD F32>F64  dst i T-SET
   loop ;

: IMP-MATERIALIZE ( ptr u8 n -- ) {: a:ptr u:n :}
   OGI# 0 ?do  a i IMP-MAT-1  loop ;

\ ---- per-op node builders ----------------------------------------------------
: IMP-ATTRS-OK ( n n -- ) {: j:n allowed:n :}  \ present attrs must be within the mask
   j OND-ATTRS@ allowed invert and 0<> if E-ONNX-ATTR throw then ;

: IMP-ARITY-OK ( n n -- ) {: j:n want:n :}
   j OND-IN# want <> if E-ONNX-ARITY throw then ;

: IMP-COMMIT ( n n n -- ) {: j:n rows:n cols:n :}   \ close the staged MIR node; bind its output
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MAKI:MIR-OP+ {: k:n :}
   j OND-OUT@ k IMP-BIND ;

\ movement nodes carry MV-PACK'd attrs and materialize only on a materialize/gathered
\ verdict (the BRIDGE-MAT rule maki/cad.f uses), so the imported IR matches MODEL: capture.
: IMP-MOVE-MAT ( n -- n ) {: attr:n :}         \ movement materialization flag from the packed verdict
   attr MAKI:MV-VD@ MAKI:MV-VD-REPORTS? if 1 else 0 then ;

: IMP-COMMIT-MOVE ( n n n n -- ) {: j:n rows:n cols:n attr:n :}   \ close a movement node
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  attr  attr IMP-MOVE-MAT  MAKI:MIR-OP+ {: k:n :}
   j OND-OUT@ k IMP-BIND ;

\ ---- internal node construction (Gemm composition builds a multi-node chain) ------
\ These commit a staged node and return its MIR node ref WITHOUT binding an ONNX name;
\ only the last node in a composed chain binds the graph output name (IMP-COMMIT).
: MK-COMPUTE ( n n -- n ) {: rows:n cols:n :}   \ close a compute node (materialized); return its ref
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MAKI:MIR-OP+ ;

: MK-MOVE ( n n n -- n ) {: rows:n cols:n attr:n :}   \ close a movement node; return its ref
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  attr  attr IMP-MOVE-MAT  MAKI:MIR-OP+ ;

\ a synthetic 1x1 f32 constant holding v (written into the arena now); returns its MIR input ref
: SYN-CONST ( r -- n ) {: v:r :}
   SYN-N @ SYN-CAP >= if E-ONNX-CAP throw then
   IMP-BUMP @ 1+ IMP-ARENA-CELLS > if E-ONNX-CAP throw then
   1 1 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI:MIR-INPUT+ {: s:n :}
   v IMP-ARENA IMP-BUMP @ T-SET
   IMP-BUMP @ SYN-N @ cells SYN-OFF + !
   s SYN-N @ cells SYN-SLOT + !
   IMP-BUMP @ 1+ IMP-BUMP !
   SYN-N @ 1+ SYN-N !
   s MAKI:MIR-IN-REF ;

\ resolve one ONNX gather index against the data operand's row count: a negative index
\ adds rows (-1 = the last row); a still-out-of-range index fails closed HERE - the float
\ bridge below must never carry a value the executor's index rounding could silently fold
\ into range (e.g. -1.0 rounding to row 0).
: GA-IDX ( n n -- n ) {: v:n rows:n :}         \ raw index + data rows -> resolved row
   v 0 < if v rows + else v then {: w:n :}
   w 0 < w rows >= or if E-ONNX-SHAPE throw then
   w ;

\ materialize an INT64 gather-index constant as a Kx1 f32 input slot: each index is
\ resolved against the data rows (GA-IDX) THEN bridged to its float value - the executor
\ reads FLOAT indices (EX-BUILD-IDX rounds), so negative-index resolution must happen at
\ import, the same way SYN-CONST synthesizes a Gemm scalar. Returns the slot's MIR input ref.
: SYN-IVEC ( n n -- n ) {: c:n rows:n :}       \ OGIC constant index + data rows -> MIR input ref
   c OGIC-NVAL@ {: k:n :}
   SYN-N @ SYN-CAP >= if E-ONNX-CAP throw then
   IMP-BUMP @ k + IMP-ARENA-CELLS > if E-ONNX-CAP throw then
   k 1 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI:MIR-INPUT+ {: s:n :}
   IMP-BUMP @ SYN-N @ cells SYN-OFF + !
   s SYN-N @ cells SYN-SLOT + !
   k 0 ?do  c i OGIC-VAL@ rows GA-IDX s>f  IMP-ARENA  IMP-BUMP @ i +  T-SET  loop
   IMP-BUMP @ k + IMP-BUMP !
   SYN-N @ 1+ SYN-N !
   s MAKI:MIR-IN-REF ;

\ Add / Mul: numpy broadcast mapped onto the capture-legal classes. The second
\ operand's shape is classified against the first (BC-CLASS, the exact mapping the
\ host executor's EX-BC@ and the planner's SHP-LEGAL? use): a full-shape operand is
\ the plain elementwise op; a 1xC second operand is a row-broadcast bias; a 1x1 is a
\ scalar. Add -> OP-ADD (full) / OP-BIAS (1xC); Mul -> OP-MUL (full) / OP-SCALE (1x1).
\ Every other numpy shape (Rx1 column, 1x1 into Add, 1xC into Mul, ragged) is outside
\ the legal classes and fails closed E-ONNX-SHAPE.
: IMP-EW-REFS ( n -- n n ) {: j:n :}           \ attr+arity gate; resolve both operands -> r0 r1
   j 0 IMP-ATTRS-OK  j 2 IMP-ARITY-OK
   j 0 OND-IN@ IMP-RESOLVE  j 1 OND-IN@ IMP-RESOLVE ;

: IMP-EW-BC ( n n -- n ) {: r0:n r1:n :}        \ operand-1 broadcast class against operand-0's RxC
   r1 IMP-REF-ROWS r1 IMP-REF-COLS  r0 IMP-REF-ROWS r0 IMP-REF-COLS  MAKI:BC-CLASS ;

: IMP-EW-BUILD ( n opkind n n -- ) {: r0:n r1:n :}   \ ( j op r0 r1 ) commit op(r0,r1); output = operand-0 shape
   MAKI:MIR-OP-BEGIN {: j:n :}                       \ op family cannot bind: consumed off the top first
   r0 MAKI:MIR-IN+  r1 MAKI:MIR-IN+
   j  r0 IMP-REF-ROWS  r0 IMP-REF-COLS  IMP-COMMIT ;

: IMP-ADD ( n -- ) {: j:n :}                   \ same shape -> OP-ADD ; 1xC second operand -> OP-BIAS
   j IMP-EW-REFS {: r0:n r1:n :}
   r0 r1 IMP-EW-BC {: bc:n :}
   bc MAKI:BC-FULL = if j MAKI-OPKIND:ADD  r0 r1 IMP-EW-BUILD exit then
   bc MAKI:BC-ROW  = if j MAKI-OPKIND:BIAS r0 r1 IMP-EW-BUILD exit then
   E-ONNX-SHAPE throw ;

: IMP-MUL ( n -- ) {: j:n :}                   \ same shape -> OP-MUL ; 1x1 second operand -> OP-SCALE
   j IMP-EW-REFS {: r0:n r1:n :}
   r0 r1 IMP-EW-BC {: bc:n :}
   bc MAKI:BC-FULL   = if j MAKI-OPKIND:MUL   r0 r1 IMP-EW-BUILD exit then
   bc MAKI:BC-SCALAR = if j MAKI-OPKIND:SCALE r0 r1 IMP-EW-BUILD exit then
   E-ONNX-SHAPE throw ;

: IMP-UNARY ( n opkind -- ) swap {: j:n :}     \ ( j op ) shape-preserving one-input op; op stays on the stack
   j 1 IMP-ARITY-OK
   j 0 OND-IN@ IMP-RESOLVE {: r0:n :}
   MAKI:MIR-OP-BEGIN  r0 MAKI:MIR-IN+
   j  r0 IMP-REF-ROWS  r0 IMP-REF-COLS  IMP-COMMIT ;

: IMP-RELU ( n -- ) {: j:n :}
   j 0 IMP-ATTRS-OK  j MAKI-OPKIND:RELU IMP-UNARY ;

: IMP-SOFTMAX ( n -- ) {: j:n :}               \ axis must be the last axis of the 2D tensor
   j ATTR-AXIS IMP-ATTRS-OK
   j OND-AXIS@ {: ax:n :}
   ax -1 <> ax 1 <> and if E-ONNX-ATTR throw then
   j MAKI-OPKIND:SOFTMAX-ROW IMP-UNARY ;

\ Gemm accepts alpha / beta / transA / transB; any other attribute rejects. Every one
\ is expressible by composition (a TRANSPOSE per transA/transB; a SCALE per non-unit
\ alpha/beta), so the value rejects that the pre-composition importer raised are gone.
: IMP-GEMM-ATTRS ( n -- ) {: j:n :}
   j ATTR-ALPHA ATTR-BETA or ATTR-TA or ATTR-TB or IMP-ATTRS-OK ;

\ default affine form (no transpose, unit alpha/beta): the single-node OP-MATMUL / OP-LINEAR path
: GEMM-SIMPLE? ( n -- bool ) {: j:n :}
   j OND-TA@ 0=  j OND-TB@ 0=  and
   j OND-ALPHA@ F32-ONE =  and  j OND-BETA@ F32-ONE =  and ;

: IMP-GEMM-SIMPLE ( n -- ) {: j:n :}           \ 2 inputs -> matmul; 3 -> linear (bias 1xN)
   j 0 OND-IN@ IMP-RESOLVE {: rx:n :}
   j 1 OND-IN@ IMP-RESOLVE {: rw:n :}
   rx IMP-REF-COLS rw IMP-REF-ROWS <> if E-ONNX-SHAPE throw then
   rx IMP-REF-ROWS {: m:n :}  rw IMP-REF-COLS {: nc:n :}
   j OND-IN# 2 = if
      MAKI-OPKIND:MATMUL MAKI:MIR-OP-BEGIN  rx MAKI:MIR-IN+  rw MAKI:MIR-IN+
      j m nc IMP-COMMIT exit then
   j 2 OND-IN@ IMP-RESOLVE {: rb:n :}
   rb IMP-REF-ROWS 1 <>  rb IMP-REF-COLS nc <>  or if E-ONNX-SHAPE throw then
   MAKI-OPKIND:LINEAR MAKI:MIR-OP-BEGIN  rx MAKI:MIR-IN+  rw MAKI:MIR-IN+  rb MAKI:MIR-IN+
   j m nc IMP-COMMIT ;

\ transA/transB: return the operand, transposed by an inserted TRANSPOSE node when the flag is set
: GEMM-T ( n -- n ) {: r:n :}                  \ insert TRANSPOSE(r) -> r^T ref
   MAKI-OPKIND:TRANSPOSE MAKI:MIR-OP-BEGIN  r MAKI:MIR-IN+
   MAKI:MV-TRANSPOSE MAKI:MVV-STAGED 0 0 MAKI:MV-PACK {: attr:n :}
   r IMP-REF-COLS  r IMP-REF-ROWS  attr  MK-MOVE ;
: GEMM-MAYBE-T ( n n -- n ) {: r:n t:n :}  t 0<> if r GEMM-T else r then ;

\ non-unit alpha/beta: insert an OP-SCALE node multiplying ref r by the scalar a (1x1 constant)
: GEMM-SCALE ( n r -- n ) {: r:n a:r :}
   a SYN-CONST {: cs:n :}
   MAKI-OPKIND:SCALE MAKI:MIR-OP-BEGIN  r MAKI:MIR-IN+  cs MAKI:MIR-IN+
   r IMP-REF-ROWS  r IMP-REF-COLS  MK-COMPUTE ;

\ apply the C operand under beta: 0 drops C, 1 adds it as a bias, else scales C by beta then adds
: GEMM-C ( n n -- n ) {: j:n acc:n :}          \ j acc -- result ref
   j OND-BETA@ 0= if acc exit then
   j 2 OND-IN@ IMP-RESOLVE {: rc:n :}
   rc IMP-REF-ROWS 1 <>  rc IMP-REF-COLS acc IMP-REF-COLS <>  or if E-ONNX-SHAPE throw then
   j OND-BETA@ F32-ONE <> if  rc j OND-BETA@ F32>F64 GEMM-SCALE  else rc  then  {: rc2:n :}
   MAKI-OPKIND:BIAS MAKI:MIR-OP-BEGIN  acc MAKI:MIR-IN+  rc2 MAKI:MIR-IN+
   acc IMP-REF-ROWS  acc IMP-REF-COLS  MK-COMPUTE ;

\ composed form: (transA?A^T:A) @ (transB?B^T:B), then alpha scale, then beta*C bias-add
: IMP-GEMM-COMPOSED ( n -- ) {: j:n :}
   j 0 OND-IN@ IMP-RESOLVE  j OND-TA@ GEMM-MAYBE-T {: ra:n :}
   j 1 OND-IN@ IMP-RESOLVE  j OND-TB@ GEMM-MAYBE-T {: rb:n :}
   ra IMP-REF-COLS rb IMP-REF-ROWS <> if E-ONNX-SHAPE throw then
   ra IMP-REF-ROWS {: m:n :}  rb IMP-REF-COLS {: nc:n :}
   MAKI-OPKIND:MATMUL MAKI:MIR-OP-BEGIN  ra MAKI:MIR-IN+  rb MAKI:MIR-IN+
   m nc MK-COMPUTE {: acc:n :}
   j OND-ALPHA@ F32-ONE <> if  acc j OND-ALPHA@ F32>F64 GEMM-SCALE  else acc  then  {: acc2:n :}
   j OND-IN# 3 = if  j acc2 GEMM-C  else acc2  then  {: acc3:n :}
   j OND-OUT@ acc3 IMP-BIND ;

: IMP-GEMM ( n -- ) {: j:n :}
   j IMP-GEMM-ATTRS
   j OND-IN# dup 2 < swap 3 > or if E-ONNX-ARITY throw then
   j GEMM-SIMPLE? if j IMP-GEMM-SIMPLE exit then
   j IMP-GEMM-COMPOSED ;

\ ---- movement op builders (MOVE-KIND coverage; wired the way MODEL: capture does) ---
\ Movement nodes are exact layout rewrites: they carry MV-PACK'd attrs + a dissolution
\ verdict and are host-executed by the same maki/move.f references the planner proves.

\ resolve one concrete reshape target dim: a positive literal stays; 0 copies the input
\ dim at that position (ONNX allowzero=0 default). -1 (infer) is handled by RS-RESOLVE.
: RS-DIM ( n n -- n ) {: d:n in:n :}           \ raw dim d, input dim in -> concrete dim
   d 0 > if d exit then
   d 0= if in exit then
   E-ONNX-SHAPE throw ;                          \ d < -1 is not a legal reshape dim

\ resolve the 2D target [R,C] from the raw shape constant [t0,t1] against the input RxC:
\ 0 copies the input dim; exactly one -1 is inferred from the element count (must divide
\ exactly); two -1 dims or a non-dividing infer fails closed (E-ONNX-SHAPE).
: RS-RESOLVE ( n n n n -- n n ) {: t0:n t1:n ir:n ic:n :}   \ t0 t1 ir ic -- tr tc
   t0 -1 = t1 -1 = and if E-ONNX-SHAPE throw then
   ir ic * {: e:n :}
   t0 -1 = if
      t1 ic RS-DIM {: c:n :}
      c 0= if E-ONNX-SHAPE throw then
      e c mod 0<> if E-ONNX-SHAPE throw then
      e c /  c  exit then
   t1 -1 = if
      t0 ir RS-DIM {: r:n :}
      r 0= if E-ONNX-SHAPE throw then
      e r mod 0<> if E-ONNX-SHAPE throw then
      r  e r /  exit then
   t0 ir RS-DIM  t1 ic RS-DIM ;

: IMP-RESHAPE ( n -- ) {: j:n :}               \ shape is an INT64 initializer; a runtime shape rejects
   j 0 IMP-ATTRS-OK  j 2 IMP-ARITY-OK
   j 1 OND-IN@ OGIC-FIND 0= if E-ONNX-DYNSHAPE throw then   \ shape not a static constant -> dynamic
   {: c:n :}
   c OGIC-NVAL@ 2 <> if E-ONNX-RANK throw then              \ the 2D IR needs a [R,C] target
   j 0 OND-IN@ IMP-RESOLVE {: r0:n :}
   c 0 OGIC-VAL@  c 1 OGIC-VAL@  r0 IMP-REF-ROWS  r0 IMP-REF-COLS  RS-RESOLVE {: tr:n tc:n :}
   r0 IMP-REF-ROWS r0 IMP-REF-COLS *  tr tc *  <> if E-ONNX-SHAPE throw then   \ element count must agree
   MAKI-OPKIND:RESHAPE MAKI:MIR-OP-BEGIN  r0 MAKI:MIR-IN+
   MAKI:MV-RESHAPE MAKI:MVV-FREE tr tc MAKI:MV-PACK {: attr:n :}   \ row-major reshape is a free rewrite
   j  tr tc  attr  IMP-COMMIT-MOVE ;

: IMP-TRANSPOSE ( n -- ) {: j:n :}             \ 2D transpose; perm absent (reverse) or exactly [1,0]
   j ATTR-PERM IMP-ATTRS-OK
   j OND-PERMN@ {: pn:n :}
   pn 0<> if
      pn 2 <> if E-ONNX-ATTR throw then                    \ 2D only: perm must be rank 2
      j OND-PERM0@ 1 <>  j OND-PERM1@ 0 <>  or if E-ONNX-ATTR throw then
   then
   j 1 IMP-ARITY-OK
   j 0 OND-IN@ IMP-RESOLVE {: r0:n :}
   MAKI-OPKIND:TRANSPOSE MAKI:MIR-OP-BEGIN  r0 MAKI:MIR-IN+
   MAKI:MV-TRANSPOSE MAKI:MVV-STAGED 0 0 MAKI:MV-PACK {: attr:n :}
   j  r0 IMP-REF-COLS  r0 IMP-REF-ROWS  attr  IMP-COMMIT-MOVE ;

: IMP-CONCAT ( n -- ) {: j:n :}                \ 2-input row concat (axis 0): RxC + SxC -> (R+S)xC
   j ATTR-AXIS IMP-ATTRS-OK
   j OND-ATTRS@ ATTR-AXIS and 0= if E-ONNX-ATTR throw then  \ Concat requires an axis
   j OND-AXIS@ 0 <> if E-ONNX-ATTR throw then               \ only axis 0 (row append) in v1
   j 2 IMP-ARITY-OK
   j 0 OND-IN@ IMP-RESOLVE {: ra:n :}
   j 1 OND-IN@ IMP-RESOLVE {: rb:n :}
   ra IMP-REF-COLS rb IMP-REF-COLS <> if E-ONNX-SHAPE throw then
   MAKI-OPKIND:CONCAT MAKI:MIR-OP-BEGIN  ra MAKI:MIR-IN+  rb MAKI:MIR-IN+
   MAKI:MV-CONCAT MAKI:MVV-MATERIALIZE 0 0 MAKI:MV-PACK {: attr:n :}
   j  ra IMP-REF-ROWS rb IMP-REF-ROWS +  ra IMP-REF-COLS  attr  IMP-COMMIT-MOVE ;

\ Slice-13 takes starts/ends (+ optional axes/steps) as rank-1 INT64 operands. They are
\ static graph constants (the OGIC table); a runtime-computed range fails E-ONNX-DYNSHAPE.
\ v1 supports a single row-axis slice: axes absent or [0], steps absent or [1], starts/ends
\ length 1 (else E-ONNX-ATTR). Indices clamp per ONNX (a negative index adds the dim; both
\ ends clamp into [0,R]); an empty or inverted clamped range fails E-ONNX-SHAPE.
: SLICE-VEC1 ( n -- n ) {: ni:n :}             \ single int of a length-1 INT64 constant operand
   ni OGIC-FIND 0= if E-ONNX-DYNSHAPE throw then  {: c:n :}
   c OGIC-NVAL@ 1 <> if E-ONNX-ATTR throw then    \ v1 slices a single axis
   c 0 OGIC-VAL@ ;

: SLICE-AXIS0 ( n -- ) {: ni:n :}  ni SLICE-VEC1 0 <> if E-ONNX-ATTR throw then ;
: SLICE-STEP1 ( n -- ) {: ni:n :}  ni SLICE-VEC1 1 <> if E-ONNX-ATTR throw then ;

: SLICE-CLAMP ( n n -- n ) {: v:n dim:n :}     \ ONNX index clamp: neg adds dim, clamp into [0,dim]
   v 0 < if v dim + else v then {: w:n :}
   w 0 < if 0 exit then
   w dim > if dim exit then
   w ;

: IMP-SLICE ( n -- ) {: j:n :}
   j 0 IMP-ATTRS-OK                             \ Slice-13 carries no attributes
   j OND-IN# {: nin:n :}
   nin 3 <  nin 5 >  or if E-ONNX-ARITY throw then
   nin 4 >= if j 3 OND-IN@ SLICE-AXIS0 then     \ axes present -> must be [0]
   nin 5 >= if j 4 OND-IN@ SLICE-STEP1 then     \ steps present -> must be [1]
   j 1 OND-IN@ SLICE-VEC1 {: st:n :}
   j 2 OND-IN@ SLICE-VEC1 {: en:n :}
   j 0 OND-IN@ IMP-RESOLVE {: r0:n :}
   r0 IMP-REF-ROWS {: rows:n :}  r0 IMP-REF-COLS {: cols:n :}
   st rows SLICE-CLAMP {: s0:n :}  en rows SLICE-CLAMP {: s1:n :}
   s0 s1 >= if E-ONNX-SHAPE throw then          \ empty (s0=s1) or inverted clamped range: v1 fail-closed
   MAKI-OPKIND:SLICE MAKI:MIR-OP-BEGIN  r0 MAKI:MIR-IN+
   MAKI:MV-SLICE  MAKI-LAYOUT:ROW s0 cols MAKI:MV-SLICE-VERDICT  s0 s1 MAKI:MV-PACK {: attr:n :}
   j  s1 s0 -  cols  attr  IMP-COMMIT-MOVE ;

\ Gather (axis 0) selects rows of the data operand by an INT64 indices operand. The
\ executor reads FLOAT indices, so the int64 constant is materialized into the float
\ arena as a Kx1 slot (SYN-IVEC, indices resolved against the data rows) at import.
\ axis absent-or-0; runtime indices -> E-ONNX-DYNSHAPE; a FLOAT indices initializer
\ (wrong dtype) -> E-ONNX-DTYPE; an out-of-range index -> E-ONNX-SHAPE (GA-IDX).
: GATHER-IDX ( n n -- n n ) {: ni:n rows:n :}  \ indices name slot + data rows -> slot ref + count
   ni OGIC-FIND if {: c:n :}
      c OGIC-NVAL@ {: k:n :}  c rows SYN-IVEC  k  exit then
   drop
   ni IMP-INIT-FIND if drop E-ONNX-DTYPE throw then   \ a FLOAT initializer: wrong indices dtype
   E-ONNX-DYNSHAPE throw ;                            \ runtime-computed indices

: IMP-GATHER ( n -- ) {: j:n :}
   j ATTR-AXIS IMP-ATTRS-OK                     \ axis is the only recognized attribute
   j OND-ATTRS@ ATTR-AXIS and 0<> if
      j OND-AXIS@ 0 <> if E-ONNX-ATTR throw then \ axis present -> must be 0
   then
   j 2 IMP-ARITY-OK
   j 0 OND-IN@ IMP-RESOLVE {: rx:n :}
   j 1 OND-IN@  rx IMP-REF-ROWS  GATHER-IDX {: ridx:n k:n :}
   MAKI-OPKIND:GATHER MAKI:MIR-OP-BEGIN  rx MAKI:MIR-IN+  ridx MAKI:MIR-IN+
   MAKI:MV-GATHER MAKI:MVV-GATHERED 0 0 MAKI:MV-PACK {: attr:n :}
   j  k  rx IMP-REF-COLS  attr  IMP-COMMIT-MOVE ;

\ one node: LOWER (compute) + MOVE-KIND (movement) are the fail-closed coverage; then IR map
: IMP-NODE ( n -- ) {: j:n :}
   j OND-OP$ {: a:ptr u:n :}
   a u s" Add"       STR= if j IMP-ADD       exit then
   a u s" Mul"       STR= if j IMP-MUL       exit then
   a u s" Relu"      STR= if j IMP-RELU      exit then
   a u s" Softmax"   STR= if j IMP-SOFTMAX   exit then
   a u s" Gemm"      STR= if j IMP-GEMM      exit then
   a u s" Reshape"   STR= if j IMP-RESHAPE   exit then
   a u s" Transpose" STR= if j IMP-TRANSPOSE exit then
   a u s" Concat"    STR= if j IMP-CONCAT    exit then
   a u s" Slice"     STR= if j IMP-SLICE     exit then
   a u s" Gather"    STR= if j IMP-GATHER    exit then
   a u LOWER 2drop                             \ unmatched: LOWER's E-MK-ONNX (Conv, etc.)
   E-MK-ONNX throw ;                           \ LOWER/IR-map drift: still fail closed

\ ---- v1 output contract: one graph output, and it is the last node -----------
: IMP-CHECK-OUT ( -- )
   OGO# 1 <> if E-ONNX-OUTPUT throw then
   MAKI:MIR-N@ 0= if E-ONNX-OUTPUT throw then
   0 OGO-NAME@ IMP-RESOLVE {: r:n :}
   r MAKI:MIR-N@ 1- <> if E-ONNX-OUTPUT throw then
   r IMP-REF-ROWS 0 OGO-ROWS@ <>  r IMP-REF-COLS 0 OGO-COLS@ <>  or
      if E-ONNX-SHAPE throw then ;

public

\ import a serialized ModelProto: parse, build the model IR, materialize weights
: IMPORT ( ptr u8 n -- ) {: a:ptr u:n :}
   IMP-RESET  MAKI:MIR-RESET
   a u OG-PARSE
   OGG$ MAKI:MIR-NAME!
   OGIN# 0 ?do  i IMP-INPUT-1  loop
   OGI#  0 ?do  i IMP-INIT-1   loop
   a u IMP-MATERIALIZE
   OND#  0 ?do  i IMP-NODE     loop
   IMP-CHECK-OUT ;

\ import a .onnx file (the model buffer is an OS mapping, process-lifetime)
: IMPORT-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE-SIZE {: sz:n :}
   sz MEM-ALLOC-BYTES {: buf:ptr cap:n :}
   a u buf cap READ-ALL {: got:n :}
   buf got IMPORT ;

\ ---- runtime inputs (the slots a caller binds before EX-RUN) ------------------
: IN# ( -- n )  IMP-IN-N @ ;

: IN-CK ( n -- n )
   dup 0 < over IMP-IN-N @ >= or if E-ONNX-IDX throw then ;

: IN-SLOT@ ( n -- n )  IN-CK cells IMP-INSLOT + @ ;
: IN-NAME$ ( n -- ptr u8 n )  IN-CK cells IMP-INGI + @  OGIN-NAME@ OGN$ ;

\ ---- materialized initializers -------------------------------------------------
: INIT# ( -- n )  OGI# ;
: INIT-SLOT@ ( n -- n )  OGI-CK cells IMP-SLOT + @ ;
: INIT-DATA@ ( n -- ptr a )  OGI-CK cells IMP-AOFF + @  IMP-ARENA swap T-AT ;

\ bind every initializer buffer + synthetic Gemm constant to its executor slot (after MAKI:EX-RESET)
: BIND-INITS ( -- )
   INIT# 0 ?do  i INIT-DATA@ i INIT-SLOT@ MAKI:EX-BIND  loop
   SYN-N @ 0 ?do
      IMP-ARENA i cells SYN-OFF + @ T-AT   i cells SYN-SLOT + @   MAKI:EX-BIND
   loop ;

\ ---- the imported model's output node -------------------------------------------
: OUT-NODE@ ( -- n )
   MAKI:MIR-N@ 0= if E-ONNX-OUTPUT throw then
   MAKI:MIR-N@ 1- ;

end-package
