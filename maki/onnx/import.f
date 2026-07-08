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
\ The IR mapping on top of that coverage: Add->OP-ADD, Mul->OP-MUL (strict
\ same-shape operands; ONNX numpy broadcast beyond that fails E-ONNX-SHAPE),
\ Relu->OP-RELU, Softmax->OP-SOFTMAX-ROW (axis must be the last: -1 or 1),
\ Gemm->OP-MATMUL (2 inputs) / OP-LINEAR (3 inputs, bias 1xN) with
\ alpha=beta=1 and transA=transB=0 enforced (E-ONNX-ATTR otherwise). Movement
\ ops (ONNX:MOVE-KIND set) are a follow-up leg and today fail closed through
\ LOWER. v1 output contract: exactly one graph output and it is the LAST node
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

: IMP-RESET ( -- )
   0 IMP-BUMP !  0 IMP-IN-N !
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
   gi OGIN-ROWS@ gi OGIN-COLS@ MAKI:DT-F32 MAKI:LAY-ROW MAKI:MIR-INPUT+ {: s:n :}
   ni s MAKI:MIR-IN-REF IMP-BIND
   s  IMP-IN-N @ cells IMP-INSLOT + !
   gi IMP-IN-N @ cells IMP-INGI + !
   IMP-IN-N @ 1+ IMP-IN-N ! ;

: IMP-INIT-1 ( n -- ) {: iz:n :}
   iz OGI-ROWS@ {: rows:n :}  iz OGI-COLS@ {: cols:n :}
   rows cols MAKI:DT-F32 MAKI:LAY-ROW MAKI:MIR-INPUT+ {: s:n :}
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
   rows cols MAKI:DT-F32 MAKI:LAY-ROW 0 1 MAKI:MIR-OP+ {: k:n :}
   j OND-OUT@ k IMP-BIND ;

: IMP-EW2 ( n n -- ) {: j:n op:n :}            \ Add / Mul: strict same-shape binary elementwise
   j 0 IMP-ATTRS-OK  j 2 IMP-ARITY-OK
   j 0 OND-IN@ IMP-RESOLVE {: r0:n :}
   j 1 OND-IN@ IMP-RESOLVE {: r1:n :}
   r0 IMP-REF-ROWS r1 IMP-REF-ROWS <>  r0 IMP-REF-COLS r1 IMP-REF-COLS <>  or
      if E-ONNX-SHAPE throw then
   op MAKI:MIR-OP-BEGIN  r0 MAKI:MIR-IN+  r1 MAKI:MIR-IN+
   j  r0 IMP-REF-ROWS  r0 IMP-REF-COLS  IMP-COMMIT ;

: IMP-UNARY ( n n -- ) {: j:n op:n :}          \ shape-preserving one-input op
   j 1 IMP-ARITY-OK
   j 0 OND-IN@ IMP-RESOLVE {: r0:n :}
   op MAKI:MIR-OP-BEGIN  r0 MAKI:MIR-IN+
   j  r0 IMP-REF-ROWS  r0 IMP-REF-COLS  IMP-COMMIT ;

: IMP-RELU ( n -- ) {: j:n :}
   j 0 IMP-ATTRS-OK  j MAKI:OP-RELU IMP-UNARY ;

: IMP-SOFTMAX ( n -- ) {: j:n :}               \ axis must be the last axis of the 2D tensor
   j ATTR-AXIS IMP-ATTRS-OK
   j OND-AXIS@ {: ax:n :}
   ax -1 <> ax 1 <> and if E-ONNX-ATTR throw then
   j MAKI:OP-SOFTMAX-ROW IMP-UNARY ;

: IMP-GEMM-ATTRS ( n -- ) {: j:n :}            \ only the default affine form lowers
   j ATTR-ALPHA ATTR-BETA or ATTR-TA or ATTR-TB or IMP-ATTRS-OK
   j OND-ALPHA@ F32-ONE <> if E-ONNX-ATTR throw then
   j OND-BETA@  F32-ONE <> if E-ONNX-ATTR throw then
   j OND-TA@ 0<>  j OND-TB@ 0<>  or if E-ONNX-ATTR throw then ;

: IMP-GEMM ( n -- ) {: j:n :}                  \ 2 inputs -> matmul; 3 -> linear (bias 1xN)
   j IMP-GEMM-ATTRS
   j OND-IN# dup 2 < swap 3 > or if E-ONNX-ARITY throw then
   j 0 OND-IN@ IMP-RESOLVE {: rx:n :}
   j 1 OND-IN@ IMP-RESOLVE {: rw:n :}
   rx IMP-REF-COLS rw IMP-REF-ROWS <> if E-ONNX-SHAPE throw then
   rx IMP-REF-ROWS {: m:n :}  rw IMP-REF-COLS {: nc:n :}
   j OND-IN# 2 = if
      MAKI:OP-MATMUL MAKI:MIR-OP-BEGIN  rx MAKI:MIR-IN+  rw MAKI:MIR-IN+
      j m nc IMP-COMMIT exit then
   j 2 OND-IN@ IMP-RESOLVE {: rb:n :}
   rb IMP-REF-ROWS 1 <>  rb IMP-REF-COLS nc <>  or if E-ONNX-SHAPE throw then
   MAKI:OP-LINEAR MAKI:MIR-OP-BEGIN  rx MAKI:MIR-IN+  rw MAKI:MIR-IN+  rb MAKI:MIR-IN+
   j m nc IMP-COMMIT ;

\ one node: the LOWER table is the fail-closed coverage gate, then the IR mapping
: IMP-NODE ( n -- ) {: j:n :}
   j OND-OP$ LOWER 2drop
   j OND-OP$ {: a:ptr u:n :}
   a u s" Add"     STR= if j MAKI:OP-ADD IMP-EW2 exit then
   a u s" Mul"     STR= if j MAKI:OP-MUL IMP-EW2 exit then
   a u s" Relu"    STR= if j IMP-RELU    exit then
   a u s" Softmax" STR= if j IMP-SOFTMAX exit then
   a u s" Gemm"    STR= if j IMP-GEMM    exit then
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

\ bind every initializer buffer to its executor input slot (after MAKI:EX-RESET)
: BIND-INITS ( -- )
   INIT# 0 ?do  i INIT-DATA@ i INIT-SLOT@ MAKI:EX-BIND  loop ;

\ ---- the imported model's output node -------------------------------------------
: OUT-NODE@ ( -- n )
   MAKI:MIR-N@ 0= if E-ONNX-OUTPUT throw then
   MAKI:MIR-N@ 1- ;

end-package
