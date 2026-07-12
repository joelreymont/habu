\ maki/backward.f - the model-IR reverse transform (CAD-PLAN section 12, dot cad-9b).
\
\ VJP substitution over the model IR: given a captured forward node table
\ (maki/model-ir.f, after MODEL: capture), walk the forward nodes in REVERSE and
\ emit each op's adjoint (maki/adjoint.f) as ORDINARY model-IR nodes APPENDED to the
\ same table - so the backward region enters the same fusion / traffic / memory
\ planners (FP-BUILD fuses a backward elementwise chain by the ordinary rules).
\
\ Cotangents are threaded by forward-node index: BW-CT[node] is the operand ref that
\ carries d(loss)/d(node-output); BW-ISG[slot] the same for a model input. The output
\ node is SEEDED with a fresh cotangent input slot. Reverse topological order means a
\ producer is visited only after all its consumers, so every partial cotangent into it
\ has already landed; a multi-use (fan-out) forward value therefore SUMS its partials
\ via emitted OP-ADD nodes. Adjoints reuse existing ops where the adjoint IS one
\ (add copies the cotangent, matmul adjoints are transposed matmuls, reshape->reshape,
\ concat->slice pair) and a dedicated OP-*-BWD op elsewhere (gelu/silu/relu/norms/rope,
\ scalar reference = the existing *-BWD word bound in the op registry).
\
\ cad-9e adds the reduce/scatter adjoints: bias/linear bias-grad -> OP-ROWSUM-BWD, scale
\ factor-grad -> OP-FULLSUM-DOT-BWD, slice input-grad -> OP-PAD-SCATTER, gather input-grad
\ -> OP-SCATTER-ADD.
\
\ Fail closed (NEVER a silent partial gradient): an op with no adjoint (cast) is a named
\ throw BEFORE any node is appended; the scale INPUT gradient fails closed (E-BW-BROADCAST)
\ on a partial broadcast (1xC / Rx1) needing a broadcast-reduce not in the op set; an empty
\ IR and an accessor used before BW-BUILD are named throws. maki -> habu; owns -5105..-5110.
\
\ Second order (higher-order grad pilot): because backward nodes are ORDINARY IR nodes,
\ a SECOND BW-BUILD differentiates the combined forward+backward region. Its seeding
\ semantics are exactly the first build's: the LAST IR node is seeded with a fresh
\ cotangent input v. After a first build over a single-output chain the last node is the
\ final accumulated input gradient g = dL/dx, so the second build computes
\ d/d(leaf) [ sum(v (.) g) ] for every differentiable leaf - the Hessian-vector product
\ H v w.r.t. x plus the mixed gradient w.r.t. the first seed s (for the gelu pilot:
\ d/dx = v (.) s (.) gelu''(x), d/ds = v (.) gelu'(x)). Each *-BWD op on the region
\ needs its own adjoint row (maki/adjoint.f); gelu-bwd is wired (BW-STEP-GELU-BWD),
\ every other *-BWD kind stays fail-closed E-BW-NOADJ until its row lands.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/op-kind.f
require maki/op-registry.f
require maki/move-facts.f
require maki/model-ir.f
require maki/adjoint.f
require maki/report.f

-5105 constant E-BW-EMPTY   \ reverse transform of an empty IR
-5106 constant E-BW-NOADJ   \ a forward op has no adjoint (cast) on the path
-5107 constant E-BW-UNSUP   \ a forward op's adjoint is v1-unsupported (named reason)
-5108 constant E-BW-STATE   \ accessor / report used before BW-BUILD
-5109 constant E-BW-CAP     \ forward node / input-slot count exceeds the cotangent tables
-5110 constant E-BW-BROADCAST \ scale input-grad needs a broadcast-reduce (partial broadcast, v1)

package MAKI
private

128 constant BW-NCAP        \ cotangent slots per node (mirrors model-ir MIR-CAP)
64  constant BW-SCAP        \ cotangent slots per input (mirrors model-ir MIR-IN-CAP)

create BW-CT  BW-NCAP cells allot    \ per forward-node output cotangent ref
create BW-ISG BW-SCAP cells allot     \ per input-slot accumulated gradient ref
create BW-CT-SET  BW-NCAP cells allot \ per-node cotangent presence
create BW-ISG-SET BW-SCAP cells allot \ per-slot gradient presence
variable BW-FWD-N                      \ forward node count snapshot (backward appends past it)
variable BW-SEED                       \ seed cotangent input slot
variable BW-BUILT?

: BW-CK ( -- )  BW-BUILT? @ 0= if E-BW-STATE throw then ;

\ input-slot bounds check (backward.f owns no slot table; validate against model-ir)
: MIR-IS-CK-OK ( n -- n )  dup 0 < over MIR-IN-SLOTS@ >= or if E-BW-STATE throw then ;

\ ---- operand-ref descriptor (input slot or producer node) -------------------
: REF-ROWS ( MIR:operand-ref -- CAD-KIND:rows ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-REF-NODE MIR-ROWS@ then ;
: REF-COLS ( MIR:operand-ref -- CAD-KIND:cols ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-REF-NODE MIR-COLS@ then ;
: REF-DT ( MIR:operand-ref -- dtype ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-DT@ else r MIR-REF-NODE MIR-DT@ then ;
: REF-LAY ( MIR:operand-ref -- layout ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-LAY@ else r MIR-REF-NODE MIR-LAY@ then ;

\ ---- cotangent table access (node ref -> BW-CT, input ref -> BW-ISG) ----------
: BW-GET ( MIR:operand-ref -- MIR:operand-ref ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT?
   if ref MIR-REF-SLOT SLOT>RAW cells BW-ISG + @
   else ref MIR-REF-NODE NODE>RAW cells BW-CT + @ then ;
: BW-SET ( MIR:operand-ref MIR:operand-ref -- )
   {: ct:MIR:operand-ref ref:MIR:operand-ref :}
   ref MIR-REF-INPUT?
   if
      ref MIR-REF-SLOT SLOT>RAW {: raw:n :}
      ct raw cells BW-ISG + !  1 raw cells BW-ISG-SET + !
   else
      ref MIR-REF-NODE NODE>RAW {: raw:n :}
      ct raw cells BW-CT + !  1 raw cells BW-CT-SET + !
   then ;
: BW-HAS? ( MIR:operand-ref -- bool ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT?
   if ref MIR-REF-SLOT SLOT>RAW cells BW-ISG-SET + @ 0<>
   else ref MIR-REF-NODE NODE>RAW cells BW-CT-SET + @ 0<> then ;

\ ---- node emitters (append one backward node; mat flag is a placeholder FP-BUILD
\ overwrites via FP-MARK, so 1 = conservative "materialize" here) --------------
\ two operands, output descriptor taken from a reference tensor (the gradient's shape)
\ the op family cannot bind into a local, so it is consumed by MIR-OP-BEGIN off the
\ top (after dref binds) before the operand refs bind.
: BW-OP2 ( MIR:operand-ref MIR:operand-ref opkind MIR:operand-ref -- MIR:operand-ref )
   {: dref:MIR:operand-ref :}
   MIR-OP-BEGIN {: a:MIR:operand-ref b:MIR:operand-ref :}
   a MIR-IN+  b MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  0  1  MIR-OP+ MIR-NODE-REF ;

\ three operands, output descriptor from a reference tensor (rope: dz cos sin)
: BW-OP3 ( MIR:operand-ref MIR:operand-ref MIR:operand-ref opkind MIR:operand-ref -- MIR:operand-ref )
   {: dref:MIR:operand-ref :}
   MIR-OP-BEGIN {: a:MIR:operand-ref b:MIR:operand-ref c:MIR:operand-ref :}
   a MIR-IN+  b MIR-IN+  c MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  0  1  MIR-OP+ MIR-NODE-REF ;

\ matmul: out rows = a.rows, out cols = b.cols (the contraction result shape)
: BW-MM ( MIR:operand-ref MIR:operand-ref -- MIR:operand-ref )
   {: a:MIR:operand-ref b:MIR:operand-ref :}
   MAKI-OPKIND:MATMUL MIR-OP-BEGIN  a MIR-IN+  b MIR-IN+
   a REF-ROWS  b REF-COLS  a REF-DT  MAKI-LAYOUT:ROW  0  1  MIR-OP+ MIR-NODE-REF ;

\ transpose: RxC -> CxR (movement node with staged verdict, like PLAN-TRANSPOSE)
: BW-TR ( MIR:operand-ref -- MIR:operand-ref ) {: s:MIR:operand-ref :}
   MV-TRANSPOSE MV-TRANSPOSE-VERDICT 0 0 MV-PACK {: attr:n :}
   MAKI-OPKIND:TRANSPOSE MIR-OP-BEGIN  s MIR-IN+
   s REF-ROWS s REF-COLS TRANSPOSE-SHAPE
   s REF-DT  s REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF ;

\ reshape the cotangent to a target shape (movement node; verdict per target
\ layout); dtype/layout come from a reference tensor like the other emitters,
\ so the families ride the stack straight into MIR-OP+
: BW-RS ( MIR:operand-ref CAD-KIND:rows CAD-KIND:cols MIR:operand-ref -- MIR:operand-ref )
   {: ct:MIR:operand-ref tr:CAD-KIND:rows tc:CAD-KIND:cols dref:MIR:operand-ref :}
   MV-RESHAPE  dref REF-LAY MV-RESHAPE-VERDICT
   tr ROWS-RAW tc COLS-RAW MV-PACK {: attr:n :}
   MAKI-OPKIND:RESHAPE MIR-OP-BEGIN  ct MIR-IN+
   tr tc  dref REF-DT  dref REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF ;

\ slice rows [r0,r1) of the cotangent; output descriptor from a reference tensor
: BW-SL ( MIR:operand-ref n n MIR:operand-ref -- MIR:operand-ref )
   {: ct:MIR:operand-ref r0:n r1:n dref:MIR:operand-ref :}
   MV-SLICE  ct REF-LAY r0 dref REF-COLS COLS-RAW MV-SLICE-VERDICT
   r0 r1 MV-PACK {: attr:n :}
   MAKI-OPKIND:SLICE MIR-OP-BEGIN  ct MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF ;

\ row-reduce the cotangent over its rows -> 1 x C (the bias / linear-bias gradient);
\ output cols/dtype/layout come from the bias reference tensor (1xC).
: BW-ROWSUM ( MIR:operand-ref MIR:operand-ref -- MIR:operand-ref )
   {: ct:MIR:operand-ref bref:MIR:operand-ref :}
   MAKI-OPKIND:ROWSUM-BWD MIR-OP-BEGIN  ct MIR-IN+
   UNIT-ROWS bref REF-COLS  bref REF-DT  bref REF-LAY  0  1  MIR-OP+ MIR-NODE-REF ;

\ full-reduce dot of the cotangent with the saved input -> 1 x 1 (the scale gradient);
\ output dtype/layout come from the scalar (scale-factor) reference tensor.
: BW-FULLSUM ( MIR:operand-ref MIR:operand-ref MIR:operand-ref -- MIR:operand-ref )
   {: ct:MIR:operand-ref x:MIR:operand-ref sref:MIR:operand-ref :}
   MAKI-OPKIND:FULLSUM-DOT-BWD MIR-OP-BEGIN  ct MIR-IN+  x MIR-IN+
   UNIT-ROWS UNIT-COLS  sref REF-DT  sref REF-LAY  0  1  MIR-OP+ MIR-NODE-REF ;

\ pad-scatter the cotangent into a zero R x C buffer at row r0 (the slice input-grad);
\ output extents from the slice's forward input, r0/r1 packed like the forward slice.
: BW-PS ( MIR:operand-ref n n MIR:operand-ref -- MIR:operand-ref )
   {: ct:MIR:operand-ref r0:n r1:n dref:MIR:operand-ref :}
   MV-SLICE MVV-MATERIALIZE r0 r1 MV-PACK {: attr:n :}
   MAKI-OPKIND:PAD-SCATTER MIR-OP-BEGIN  ct MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF ;

\ scatter-add the cotangent rows into a zero R x C buffer at the gathered indices (the
\ gather input-grad); reads the index operand, output extents from the gather's input.
: BW-SA ( MIR:operand-ref MIR:operand-ref MIR:operand-ref -- MIR:operand-ref )
   {: ct:MIR:operand-ref idx:MIR:operand-ref dref:MIR:operand-ref :}
   MV-GATHER MVV-MATERIALIZE 0 0 MV-PACK {: attr:n :}
   MAKI-OPKIND:SCATTER-ADD MIR-OP-BEGIN  ct MIR-IN+  idx MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF ;

\ ---- accumulate a new cotangent ref into a target operand (fan-out -> OP-ADD sum) --
: BW-ACCUM ( MIR:operand-ref MIR:operand-ref -- )
   {: nc:MIR:operand-ref ref:MIR:operand-ref :}
   ref BW-HAS? 0= if nc ref BW-SET exit then
   ref BW-GET {: cur:MIR:operand-ref :}
   cur nc MAKI-OPKIND:ADD ref BW-OP2  ref BW-SET ;

\ ---- per-adjoint emitters ( fn ct -- ) --------------------------------------
\ linear op: the cotangent copies unchanged to every masked input (add / residual)
: BW-STEP-COPY ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn MIR-IN-COUNT@ 0 ?do
      fn MIR-OP@ i ADJ-GRAD-IN? if
         ct  fn i MIR-INPUT-IDX MIR-IN@  BW-ACCUM
      then
   loop ;

\ product rule: dx = ct*y, dy = ct*x (both operands saved)
: BW-STEP-MUL ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: y:MIR:operand-ref :}
   ct y MAKI-OPKIND:MUL x BW-OP2  x BW-ACCUM
   ct x MAKI-OPKIND:MUL y BW-OP2  y BW-ACCUM ;

\ dedicated elementwise / reduction backward op over (ct, saved-input)
: BW-STEP-UNARY ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   ct x  fn MIR-OP@ ADJ-BWD-KIND  x BW-OP2  x BW-ACCUM ;

\ softmax adjoint reads the saved OUTPUT row (the forward node itself)
: BW-STEP-SOFTMAX ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   ct fn MIR-NODE-REF MAKI-OPKIND:SOFTMAX-ROW-BWD x BW-OP2  x BW-ACCUM ;

\ rope adjoint rotates the cotangent by -angle, reading cos/sin (operands 1,2)
: BW-STEP-ROPE ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: c:MIR:operand-ref :}
   fn 2 MIR-INPUT-IDX MIR-IN@ {: s:MIR:operand-ref :}
   ct c s MAKI-OPKIND:ROPE-BWD x BW-OP3  x BW-ACCUM ;

\ matmul adjoints are transposed matmuls: dX = ct @ Wt, dW = Xt @ ct
: BW-STEP-MATMUL ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: w:MIR:operand-ref :}
   ct  w BW-TR  BW-MM  x BW-ACCUM
   x BW-TR  ct  BW-MM  w BW-ACCUM ;

\ reshape adjoint reshapes the cotangent back to the input shape
: BW-STEP-RESHAPE ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   ct  x REF-ROWS  x REF-COLS  x  BW-RS  x BW-ACCUM ;

\ transpose adjoint transposes the cotangent (self-inverse)
: BW-STEP-TRANSPOSE ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   ct BW-TR  x BW-ACCUM ;

\ concat adjoint splits the cotangent into one slice per input (row ranges)
: BW-STEP-CONCAT ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: a:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: b:MIR:operand-ref :}
   a REF-ROWS ROWS-RAW {: ra:n :}  b REF-ROWS ROWS-RAW {: rb:n :}
   ct 0 ra a BW-SL  a BW-ACCUM
   ct ra  ra rb +  b BW-SL  b BW-ACCUM ;

\ bias adjoint: dx = ct (copy) ; d-bias = row-reduce of ct over its broadcast rows -> 1xC
: BW-STEP-BIAS ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: b:MIR:operand-ref :}
   ct x BW-ACCUM                              \ dx = cotangent copy
   ct b BW-ROWSUM  b BW-ACCUM ;               \ d-bias = rowsum(ct) -> 1 x C

\ scale adjoint: dx = ct*s, d-scale depends on the scale-operand shape. A same-shape
\ operand is the elementwise product rule (OP-MUL both); a 1x1 scalar is broadcast-scale
\ (dx = OP-SCALE(ct,s), d-scale = OP-FULLSUM-DOT-BWD(ct,x) -> 1x1); a partial broadcast
\ (1xC / Rx1) needs a broadcast-reduce not in v1 -> fail closed (E-BW-BROADCAST).
: BW-STEP-SCALE ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: s:MIR:operand-ref :}
   s REF-ROWS x REF-ROWS ROWS-EQUAL?  s REF-COLS x REF-COLS COLS-EQUAL?  and if
      ct s MAKI-OPKIND:MUL x BW-OP2  x BW-ACCUM        \ dx = ct*s (elementwise)
      ct x MAKI-OPKIND:MUL s BW-OP2  s BW-ACCUM        \ d-scale = ct*x (elementwise)
      exit
   then
   s REF-ROWS 1 ROWS-IS?  s REF-COLS 1 COLS-IS?  and 0= if E-BW-BROADCAST throw then
   ct s MAKI-OPKIND:SCALE x BW-OP2  x BW-ACCUM         \ dx = scale(ct, s) (broadcast-by-1x1)
   ct x s BW-FULLSUM  s BW-ACCUM ;            \ d-scale = fullsum-dot(ct, x) -> 1x1

\ linear adjoint: the matmul adjoints for x and w plus the bias row-reduce.
\ dX = ct @ Wt, dW = Xt @ ct, dB = rowsum(ct)
: BW-STEP-LINEAR ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: w:MIR:operand-ref :}
   fn 2 MIR-INPUT-IDX MIR-IN@ {: b:MIR:operand-ref :}
   ct  w BW-TR  BW-MM  x BW-ACCUM             \ dX = ct @ Wt
   x BW-TR  ct  BW-MM  w BW-ACCUM             \ dW = Xt @ ct
   ct b BW-ROWSUM  b BW-ACCUM ;               \ dB = rowsum(ct) -> 1 x N

\ slice adjoint: pad-scatter the cotangent into a zero buffer at the forward slice offset
\ (r0/r1 read back from the forward node's packed attrs).
: BW-STEP-SLICE ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn MIR-ATTR@ {: attr:n :}
   ct  attr MV-PA@  attr MV-PB@  x  BW-PS  x BW-ACCUM ;

\ gather adjoint: scatter-add the cotangent rows back to the gathered indices (operand 1);
\ duplicates accumulate. The index operand carries no gradient (mask bit 1 = 0).
: BW-STEP-GATHER ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: idx:MIR:operand-ref :}
   ct idx x  BW-SA  x BW-ACCUM ;

\ gelu-bwd adjoint (second order): the node computes z = dz * gelu'(x) over (dz, x).
\ d-dz = ct * gelu'(x)      = OP-GELU-BWD(ct, x)        (reuses the first derivative)
\ d-x  = ct * dz * gelu''(x) = OP-GELU-BWD2(ct*dz, x)   (needs the second derivative)
: BW-STEP-GELU-BWD ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: dz:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   ct x MAKI-OPKIND:GELU-BWD dz BW-OP2  dz BW-ACCUM
   ct dz MAKI-OPKIND:MUL x BW-OP2
   x MAKI-OPKIND:GELU-BWD2 x BW-OP2  x BW-ACCUM ;

\ ---- one forward node's reverse step ---------------------------------------
: BW-STEP ( CAD-KIND:node-id -- ) {: fn:CAD-KIND:node-id :}
   fn MIR-NODE-REF {: out:MIR:operand-ref :}
   out BW-HAS? 0= if exit then               \ node not on the backward path
   out BW-GET {: ct:MIR:operand-ref :}
   \ dispatch straight on the forward op family (exhaustive: adding an op forces an
   \ adjoint decision here). Non-differentiable ops and synthesized backward ops
   \ without an adjoint row throw; gelu-bwd carries the second-order pilot.
   fn ct  fn MIR-OP@ MATCH opkind
      add             OF BW-STEP-COPY      ENDOF
      residual-add    OF BW-STEP-COPY      ENDOF
      mul             OF BW-STEP-MUL       ENDOF
      relu            OF BW-STEP-UNARY     ENDOF
      gelu            OF BW-STEP-UNARY     ENDOF
      silu            OF BW-STEP-UNARY     ENDOF
      layernorm       OF BW-STEP-UNARY     ENDOF
      rmsnorm         OF BW-STEP-UNARY     ENDOF
      softmax-row     OF BW-STEP-SOFTMAX   ENDOF
      rope            OF BW-STEP-ROPE      ENDOF
      matmul          OF BW-STEP-MATMUL    ENDOF
      reshape         OF BW-STEP-RESHAPE   ENDOF
      transpose       OF BW-STEP-TRANSPOSE ENDOF
      concat          OF BW-STEP-CONCAT    ENDOF
      bias            OF BW-STEP-BIAS      ENDOF
      scale           OF BW-STEP-SCALE     ENDOF
      linear          OF BW-STEP-LINEAR    ENDOF
      slice           OF BW-STEP-SLICE     ENDOF
      gather          OF BW-STEP-GATHER    ENDOF
      cast            OF E-BW-UNSUP throw  ENDOF
      relu-bwd        OF E-BW-UNSUP throw  ENDOF
      gelu-bwd        OF BW-STEP-GELU-BWD  ENDOF
      gelu-bwd2       OF E-BW-UNSUP throw  ENDOF
      silu-bwd        OF E-BW-UNSUP throw  ENDOF
      layernorm-bwd   OF E-BW-UNSUP throw  ENDOF
      rmsnorm-bwd     OF E-BW-UNSUP throw  ENDOF
      softmax-row-bwd OF E-BW-UNSUP throw  ENDOF
      rope-bwd        OF E-BW-UNSUP throw  ENDOF
      rowsum-bwd      OF E-BW-UNSUP throw  ENDOF
      fullsum-dot-bwd OF E-BW-UNSUP throw  ENDOF
      pad-scatter     OF E-BW-UNSUP throw  ENDOF
      scatter-add     OF E-BW-UNSUP throw  ENDOF
   ;MATCH ;

\ ---- supported-op gate (usable BEFORE build to classify not-run) -------------
: BW-OK-OP? ( opkind -- bool )  dup ADJ-HAS?  swap ADJ-SUP?  and ;

public

\ first forward NODE lacking a supported adjoint, or -1 (scans forward nodes only);
\ returns the node index (the op family is refetched at the use site).
: BW-FIRST-BAD ( -- n )
   BW-BUILT? @ if BW-FWD-N @ else MIR-N@ then {: n:n :}
   n 0 ?do  i MIR-NODE-ID MIR-OP@ BW-OK-OP? 0= if i unloop exit then  loop  -1 ;

: BW-CAN? ( -- bool )  BW-FIRST-BAD 0< ;

private

: BW-RESET-TABLES ( -- )
   BW-NCAP 0 ?do  0 i cells BW-CT-SET + !  loop
   BW-SCAP 0 ?do  0 i cells BW-ISG-SET + !  loop ;

\ seed the output node (last forward node) with a fresh cotangent input slot
: BW-SEED-OUTPUT ( -- )
   BW-FWD-N @ 1- MIR-NODE-ID {: out:CAD-KIND:node-id :}
   out MIR-ROWS@ out MIR-COLS@ out MIR-DT@ out MIR-LAY@ MIR-INPUT+
      {: sslot:MIR:input-slot :}
   sslot BW-SEED !
   sslot MIR-IN-REF  out MIR-NODE-REF  BW-SET ;

public

\ ---- build the backward IR over the current forward node table ---------------
: BW-BUILD ( -- )
   0 BW-BUILT? !
   MIR-N@ 0= if E-BW-EMPTY throw then
   MIR-N@ BW-NCAP > MIR-IN-SLOTS@ BW-SCAP >= or if E-BW-CAP throw then
   BW-CAN? 0= if
      BW-FIRST-BAD {: bad:n :}
      bad MIR-NODE-ID MIR-OP@ ADJ-HAS? if E-BW-UNSUP else E-BW-NOADJ then throw
   then
   MIR-N@ BW-FWD-N !
   BW-RESET-TABLES
   BW-SEED-OUTPUT
   BW-FWD-N @ 0 ?do  BW-FWD-N @ 1- i - MIR-NODE-ID BW-STEP  loop
   -1 BW-BUILT? ! ;

\ ---- accessors --------------------------------------------------------------
: BW-FWD-N@     ( -- n )      BW-CK BW-FWD-N @ ;
: BW-BWD-COUNT  ( -- n )      BW-CK MIR-N@ BW-FWD-N @ - ;
: BW-SEED-SLOT@ ( -- MIR:input-slot )  BW-CK BW-SEED @ ;
: BW-NODE-CT@ ( CAD-KIND:node-id -- MIR:operand-ref )
   BW-CK dup NODE>RAW {: raw:n :}
   raw 0 < raw BW-FWD-N @ >= or if E-BW-STATE throw then
   MIR-NODE-REF dup BW-HAS? 0= if E-BW-STATE throw then BW-GET ;
: BW-SLOT-GRAD@ ( MIR:input-slot -- MIR:operand-ref )
   BW-CK dup SLOT>RAW MIR-IS-CK-OK drop
   MIR-IN-REF dup BW-HAS? 0= if E-BW-STATE throw then BW-GET ;
: BW-HAS-GRAD? ( MIR:input-slot -- bool )
   BW-CK dup SLOT>RAW MIR-IS-CK-OK drop MIR-IN-REF BW-HAS? ;

\ ---- report: the seed, the backward node count, and each model input's gradient --
: BW-SEED-ROW$ ( -- ptr u8 n )
   SB-RESET s" backward.seed: input " SB-APPEND BW-SEED @ SLOT>RAW SB-INT
   s"  (cotangent for node " SB-APPEND BW-FWD-N @ 1- SB-INT s" )" SB-APPEND SB$ ;

: BW-COUNT-ROW$ ( -- ptr u8 n )
   SB-RESET s" backward.nodes: fwd=" SB-APPEND BW-FWD-N @ SB-INT
   s"  bwd=" SB-APPEND MIR-N@ BW-FWD-N @ - SB-INT SB$ ;

: BW-GRAD-ROW$ ( MIR:input-slot -- ptr u8 n ) {: s:MIR:input-slot :}
   SB-RESET s" backward.grad: input " SB-APPEND s SLOT>RAW SB-INT
   s MIR-IN-REF {: ref:MIR:operand-ref :}
   ref BW-HAS? 0= if s"  none" SB-APPEND SB$ exit then
   ref BW-GET {: g:MIR:operand-ref :}
   g MIR-REF-INPUT?
   if s"  <- input " SB-APPEND g MIR-REF-SLOT SLOT>RAW SB-INT
   else s"  <- node " SB-APPEND g MIR-REF-NODE NODE>RAW SB-INT then  SB$ ;

: BW-INTO ( report -- report )
   BW-CK
   BW-SEED-ROW$ REPORT:WARN+
   BW-COUNT-ROW$ REPORT:WARN+
   MIR-IN-SLOTS@ 0 ?do
      i MIR-SLOT-ID {: s:MIR:input-slot :}
      s BW-SEED @ MIR-SLOT= 0= if s BW-GRAD-ROW$ REPORT:WARN+ then
   loop ;

end-package
