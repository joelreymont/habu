\ maki/backward.f - the model-IR reverse transform (docs/archive/cad-plan.md section 12, dot cad-9b).
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
require maki/mha-op.f                 \ fused-attention geometry and combined-gradient contract
require maki/spec.f                   \ the equation registry (EQ-DIFF?/EQ-ADJ@): pre-derived adjoints
require maki/prec-attr.f              \ CPREC-PAYLOAD@/CPREC@/CPREC-PACK: the equation node's attrs cell
require maki/report.f

\ ---- audited count projection (MIR typed item-count -> raw loop/compare cell) ---
\ The MIR NODE-/SLOT-COUNT@ accessors return CAD-NUM:item-count; a ?do bound, a
\ capacity compare, and a count snapshot need the raw cell the typed form cannot
\ flow into. Reopen the unsealed CAD-NUM package for one checked bridge over its
\ private ITEM-COUNT>N projection (no new TRUSTED), mirroring cad.f CAD-IX>N.
package CAD-NUM public
: BW-IC>N ( CAD-NUM:item-count -- n )  ITEM-COUNT>N ;
;package

-5105 constant E-BW-EMPTY   \ reverse transform of an empty IR
-5106 constant E-BW-NOADJ   \ a forward op has no adjoint (cast) on the path
-5107 constant E-BW-UNSUP   \ a forward op's adjoint is v1-unsupported (named reason)
-5108 constant E-BW-STATE   \ accessor / report used before BW-BUILD
-5109 constant E-BW-CAP     \ forward node / input-slot count exceeds the cotangent tables
-5110 constant E-BW-BROADCAST \ scale input-grad needs a broadcast-reduce (partial broadcast, v1)

package MAKI
private

\ Model-proportional cotangent tables (dot habu-size-model-proportional): the per-node output-cotangent
\ column, the per-slot gradient column, and their presence-flag siblings all derive their size from the
\ model at BW-BUILD entry (single-phase: written only within the build, after the bind) and grow past
\ their seeds, reusing the largest region across models. The old caps become generous sanity CEILINGS:
\ a model past them dies E-BW-CAP before any bind, prior tables intact (transactional).
$80000 constant BW-NCAP     \ generous per-node cotangent-table ceiling (nodes; was 128)
$8000  constant BW-SCAP     \ generous per-slot gradient-table ceiling (slots; was 64)
64     constant BW-CT-SEED  \ per-node presence-flag table SEED (nodes); grows past this
32     constant BW-SCT-SEED \ per-slot presence-flag table SEED (slots); grows past this

DEFER-LAYOUT-BUFFER BW-CT-AT  MIR:operand-ref \ per forward-node output cotangent ref ( n -- ptr MIR:operand-ref )
DEFER-LAYOUT-BUFFER BW-ISG-AT MIR:operand-ref \ per input-slot accumulated gradient ref ( n -- ptr MIR:operand-ref )
create BW-CT-SET  BW-CT-SEED  cells allot \ per-node cotangent presence SEED; BW-CT-SET-P starts here
create BW-ISG-SET BW-SCT-SEED cells allot \ per-slot gradient presence SEED; BW-ISG-SET-P starts here
variable BW-CT-SET-P    BW-CT-SET  BW-CT-SET-P !    \ presence-table base ptrs (grow-to-largest)
variable BW-ISG-SET-P   BW-ISG-SET BW-ISG-SET-P !
variable BW-CT-SET-CAP  BW-CT-SEED  BW-CT-SET-CAP !  \ presence-table capacities (grow-to-largest)
variable BW-ISG-SET-CAP BW-SCT-SEED BW-ISG-SET-CAP !
variable BW-CT-N                       \ live per-node cotangent-table size of the current build (exact)
variable BW-ISG-N                      \ live per-slot gradient-table size of the current build (exact)
variable BW-FWD-N                      \ forward node count snapshot (backward appends past it)
TYPED-VARIABLE BW-SEED MIR:input-slot  \ seed cotangent input slot ( -- ptr MIR:input-slot )
variable BW-BUILT?

\ Bind the cotangent tables from the model's counts at BW-BUILD entry. Each carries the generous
\ sanity ceiling (E-BW-CAP, transactional: die before any bind/allot) and records the live count.
: BW-CT-BIND ( n -- ) {: n:n :}          \ per-node output-cotangent tables sized to n forward nodes
   n BW-NCAP > if E-BW-CAP throw then
   n BW-CT-AT-BIND                                \ DEFER typed column
   n BW-CT-SET-CAP @ > if here {: b:ptr :}  n cells allot  b BW-CT-SET-P !  n BW-CT-SET-CAP ! then
   n BW-CT-N ! ;
: BW-ISG-BIND ( n -- ) {: n:n :}         \ per-slot gradient tables sized to n+1 slots (room for the seed slot)
   n 1+ {: m:n :}
   m BW-SCAP > if E-BW-CAP throw then
   m BW-ISG-AT-BIND
   m BW-ISG-SET-CAP @ > if here {: b:ptr :}  m cells allot  b BW-ISG-SET-P !  m BW-ISG-SET-CAP ! then
   m BW-ISG-N ! ;

: BW-CK ( -- )  BW-BUILT? @ 0= if E-BW-STATE throw then ;

\ input-slot bounds check (backward.f owns no slot table; validate against model-ir)
: MIR-IS-CK-OK ( n -- n )  dup 0 < over SLOT-COUNT@ CAD-NUM:BW-IC>N >= or if E-BW-STATE throw then ;

\ ---- operand-ref descriptor (input slot or producer node) -------------------
: REF-ROWS ( MIR:operand-ref -- CAD-KIND:rows ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-REF-NODE MIR-ROWS@ then ;
: REF-COLS ( MIR:operand-ref -- CAD-KIND:cols ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-REF-NODE MIR-COLS@ then ;
: REF-DT ( MIR:operand-ref -- datatype ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-DT@ else r MIR-REF-NODE MIR-DT@ then ;
: REF-LAY ( MIR:operand-ref -- layout ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-LAY@ else r MIR-REF-NODE MIR-LAY@ then ;

\ ---- cotangent table access (node ref -> BW-CT, input ref -> BW-ISG) ----------
: BW-GET ( MIR:operand-ref -- MIR:operand-ref ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT?
   if ref MIR-REF-SLOT SLOT>RAW BW-ISG-AT @
   else ref MIR-REF-NODE NODE>RAW BW-CT-AT @ then ;
: BW-SET ( MIR:operand-ref MIR:operand-ref -- )
   {: ct:MIR:operand-ref ref:MIR:operand-ref :}
   ref MIR-REF-INPUT?
   if
      ref MIR-REF-SLOT SLOT>RAW {: raw:n :}
      ct raw BW-ISG-AT !  1 BW-ISG-SET-P @ raw T-AT !
   else
      ref MIR-REF-NODE NODE>RAW {: raw:n :}
      ct raw BW-CT-AT !  1 BW-CT-SET-P @ raw T-AT !
   then ;
: BW-HAS? ( MIR:operand-ref -- bool ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT?
   if ref MIR-REF-SLOT SLOT>RAW BW-ISG-SET-P @ swap T-AT @ 0<>
   else ref MIR-REF-NODE NODE>RAW BW-CT-SET-P @ swap T-AT @ 0<> then ;

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
   tr tc MV-PACK-SHAPE {: attr:n :}
   MAKI-OPKIND:RESHAPE MIR-OP-BEGIN  ct MIR-IN+
   tr tc  dref REF-DT  dref REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF ;

\ slice rows [r0,r1) of the cotangent; output descriptor from a reference tensor
: BW-SL ( MIR:operand-ref CAD-KIND:rows CAD-KIND:rows MIR:operand-ref -- MIR:operand-ref )
   {: ct:MIR:operand-ref r0:CAD-KIND:rows r1:CAD-KIND:rows dref:MIR:operand-ref :}
   MV-SLICE  ct REF-LAY r0 dref REF-COLS MV-SLICE-VD
   r0 r1 MV-PACK-ROWS {: attr:n :}
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

\ emit a no-affine LAYERNORM node over x (recompute xhat in the backward pass; SAVE-INPUT).
: BW-LN-FWD ( MIR:operand-ref -- MIR:operand-ref ) {: x:MIR:operand-ref :}
   MAKI-OPKIND:LAYERNORM MIR-OP-BEGIN  x MIR-IN+
   x REF-ROWS x REF-COLS x REF-DT x REF-LAY  0  1  MIR-OP+ MIR-NODE-REF ;

\ LAYERNORM adjoint, dispatched on the explicit form (never the input count). plain:
\ dx = LN-BWD(dy, x). affine (y = gamma*xhat+beta): dgamma = rowsum(dy*xhat),
\ dbeta = rowsum(dy), dx = LN-BWD(dy*gamma, x) - the stage-1 golden math (maki/layernorm.f
\ LN-AFFINE-BWD), xhat recomputed from the saved x via BW-LN-FWD. dy*gamma is an OP-BCAST-MUL
\ (RxC data * 1xC gamma): its SHP-ROW-OK? legality survives BIND-SHAPES re-propagation, which
\ a same-shape OP-MUL would fail (E-CAD-PARAM-SHAPE) - the per-channel broadcast the adjoint needs.
: BW-STEP-LAYERNORM ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn LN-AFFINE? 0= if
      ct x MAKI-OPKIND:LAYERNORM-BWD x BW-OP2  x BW-ACCUM  exit
   then
   fn 1 MIR-INPUT-IDX MIR-IN@ {: g:MIR:operand-ref :}
   fn 2 MIR-INPUT-IDX MIR-IN@ {: b:MIR:operand-ref :}
   x BW-LN-FWD {: xh:MIR:operand-ref :}                      \ xhat = layernorm(x)
   ct xh MAKI-OPKIND:MUL x BW-OP2  g BW-ROWSUM  g BW-ACCUM   \ dgamma = rowsum(dy*xhat) -> 1xC (both RxC)
   ct b BW-ROWSUM  b BW-ACCUM                                \ dbeta  = rowsum(dy)       -> 1xC
   ct g MAKI-OPKIND:BCAST-MUL x BW-OP2                       \ dxhat = dy*gamma (RxC * 1xC row-broadcast)
   x MAKI-OPKIND:LAYERNORM-BWD x BW-OP2  x BW-ACCUM ;        \ dx = LN-BWD(dxhat, x)

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
   a REF-ROWS {: ra:CAD-KIND:rows :}  b REF-ROWS {: rb:CAD-KIND:rows :}
   ct ZERO-ROWS ra a BW-SL  a BW-ACCUM
   ct ra  ra rb ROWS+  b BW-SL  b BW-ACCUM ;

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

\ 1xC broadcast-multiply adjoint: y = x * g with g a 1xC row vector (dot habu-add-1xc-broadcast).
\ dx = ct * g (a 1xC-broadcast multiply, OP-BCAST-MUL again - the exact per-column analog of
\ scale's OP-SCALE dx); d-g = rowsum over rows of (ct .* x) -> 1xC (OP-MUL then OP-ROWSUM-BWD,
\ mirroring the layernorm dgamma path), so no new backward op is needed.
: BW-STEP-BCAST-MUL ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: g:MIR:operand-ref :}
   ct g MAKI-OPKIND:BCAST-MUL x BW-OP2  x BW-ACCUM        \ dx = ct * g (1xC broadcast)
   ct x MAKI-OPKIND:MUL x BW-OP2  g BW-ROWSUM  g BW-ACCUM ; \ d-g = rowsum(ct .* x) -> 1xC

\ dropout adjoint (dot habu-dropout-op-train): dx = mask*scale*ct - the SAME masked scale the
\ forward node applied. One OP-DROPOUT-BWD node reads dy (operand 0) and the FORWARD node
\ (operand 1, like softmax reads its saved output): the executor recovers the forward node's
\ table index from that ref and reseeds the mask stream, so the mask matches bit-for-bit and
\ dropped elements get zero gradient. The forward mode/pfix attr rides through unchanged (eval
\ makes the VJP the identity dy->dx). Only the data input receives a gradient.
: BW-STEP-DROPOUT ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn MIR-ATTR@ {: attr:n :}                                 \ copy mode/pfix onto the VJP node
   MAKI-OPKIND:DROPOUT-BWD MIR-OP-BEGIN
   ct MIR-IN+  fn MIR-NODE-REF MIR-IN+                        \ dy + the forward node (for its seed index)
   x REF-ROWS x REF-COLS x REF-DT x REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF {: dx:MIR:operand-ref :}
   dx x BW-ACCUM ;

\ recompute silu(gate) as a plain OP-SILU node over the saved gate (recompute-over-save,
\ like BW-LN-FWD recomputes xhat); output descriptor from the gate reference.
: BW-SILU-FWD ( MIR:operand-ref -- MIR:operand-ref ) {: g:MIR:operand-ref :}
   MAKI-OPKIND:SILU MIR-OP-BEGIN  g MIR-IN+
   g REF-ROWS g REF-COLS g REF-DT g REF-LAY  0  1  MIR-OP+ MIR-NODE-REF ;

\ SwiGLU adjoint (dot habu-infer-swiglu-op): y = silu(gate)*up. Both operands receive a
\ gradient; the closed form decomposes into ops that are ALL registry-complete, so no
\ dedicated *-BWD op-kind (the bcast-mul precedent). With ct the output cotangent:
\   d_gate = ct*up*silu'(gate) = OP-SILU-BWD(ct*up, gate)  (ct*up is one OP-MUL)
\   d_up   = ct*silu(gate)     = OP-MUL(ct, OP-SILU(gate)) (silu(gate) recomputed above)
: BW-STEP-SWIGLU ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: g:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: u:MIR:operand-ref :}
   ct u MAKI-OPKIND:MUL g BW-OP2  g MAKI-OPKIND:SILU-BWD g BW-OP2  g BW-ACCUM \ d_gate = silu-bwd(ct*up, gate)
   ct  g BW-SILU-FWD  MAKI-OPKIND:MUL u BW-OP2  u BW-ACCUM ;                   \ d_up   = ct * silu(gate)

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

\ segment causal attention adjoint (dot BTC-1): one fused OP-SEG-ATTN-BWD node reads
\ Q,K,V and the cotangent dO and emits the combined [dQ;dK;dV] as 3*(B*T) rows (the
\ block width / causal flag ride through the attrs cell unchanged, like the slice
\ adjoint reads its forward attrs). Three slices split it back into dQ,dK,dV, one per
\ input. Masked positions carry zero grad structurally inside the fused op, so there
\ is no separate mask adjoint.
: BW-STEP-SEGMENT-ATTN ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: q:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: k:MIR:operand-ref :}
   fn 2 MIR-INPUT-IDX MIR-IN@ {: v:MIR:operand-ref :}
   fn MIR-ATTR@ {: attr:n :}
   q REF-ROWS {: bt:CAD-KIND:rows :}        \ B*T (one input's rows)
   bt bt ROWS+ {: bt2:CAD-KIND:rows :}       \ 2*B*T
   bt2 bt ROWS+ {: bt3:CAD-KIND:rows :}      \ 3*B*T (combined [dQ;dK;dV] rows)
   MAKI-OPKIND:SEG-ATTN-BWD MIR-OP-BEGIN
   q MIR-IN+  k MIR-IN+  v MIR-IN+  ct MIR-IN+
   bt3 q REF-COLS q REF-DT q REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF {: g:MIR:operand-ref :}
   g ZERO-ROWS bt  q BW-SL  q BW-ACCUM       \ dQ = combined rows [0,   B*T)
   g bt  bt2       k BW-SL  k BW-ACCUM        \ dK = combined rows [B*T, 2*B*T)
   g bt2 bt3       v BW-SL  v BW-ACCUM ;      \ dV = combined rows [2*B*T,3*B*T)

\ fused multi-head causal self-attention adjoint (dot habu-op-mha-fused): one fused
\ OP-MHA-BWD node emits the combined [dX|dWqkv|dbqkv|dWo|dbo] as one (TOTAL x 1) buffer (the
\ head count / seq length / causal flag ride through the attrs cell unchanged, like seg-attn).
\ Five slices at the mha-op.f cumulative CELL boundaries split it back into one gradient per
\ forward input, each shaped by that input's own descriptor - the seg-attn combined-buffer
\ slice pattern, generalized to five differently-shaped gradients. SELF-CONTAINED: the node
\ carries ALL FIVE forward inputs plus the cotangent dY (forward inputs first, cotangent last -
\ the seg-attn operand order), so EX-MHA-BWD re-runs MHA-FWD from this node's own inputs before
\ MHA-BWD - a two-MHA model never differentiates one node from another's tape.
: BW-ELEM-ROWS ( MIR:operand-ref -- CAD-KIND:rows ) {: r:MIR:operand-ref :}
   r REF-ROWS r REF-COLS SHAPE-ELEMS DIM>ROWS ;
: BW-STEP-MHA ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   fn 1 MIR-INPUT-IDX MIR-IN@ {: wqkv:MIR:operand-ref :}
   fn 2 MIR-INPUT-IDX MIR-IN@ {: bqkv:MIR:operand-ref :}
   fn 3 MIR-INPUT-IDX MIR-IN@ {: wo:MIR:operand-ref :}
   fn 4 MIR-INPUT-IDX MIR-IN@ {: bo:MIR:operand-ref :}
   fn MIR-ATTR@ {: attr:n :}
   x BW-ELEM-ROWS {: xend:CAD-KIND:rows :}
   xend wqkv BW-ELEM-ROWS ROWS+ {: wqend:CAD-KIND:rows :}
   wqend bqkv BW-ELEM-ROWS ROWS+ {: bqend:CAD-KIND:rows :}
   bqend wo BW-ELEM-ROWS ROWS+ {: woend:CAD-KIND:rows :}
   woend bo BW-ELEM-ROWS ROWS+ {: total:CAD-KIND:rows :}
   MAKI-OPKIND:MHA-BWD MIR-OP-BEGIN
   x MIR-IN+  wqkv MIR-IN+  bqkv MIR-IN+  wo MIR-IN+  bo MIR-IN+  ct MIR-IN+  \ five forward inputs, then dY
   total UNIT-COLS  x REF-DT x REF-LAY  attr  1  MIR-OP+ MIR-NODE-REF {: g:MIR:operand-ref :}
   g ZERO-ROWS xend  x    BW-SL  x    BW-ACCUM \ dX
   g xend wqend      wqkv BW-SL  wqkv BW-ACCUM \ dWqkv
   g wqend bqend     bqkv BW-SL  bqkv BW-ACCUM \ dbqkv
   g bqend woend     wo   BW-SL  wo   BW-ACCUM \ dWo
   g woend total     bo   BW-SL  bo   BW-ACCUM ; \ dbo

\ equation adjoint (docs/model-unified.md stage 2): the forward node's attrs cell carries
\ the equation's spec-registry slot; each factor's gradient is a PRE-DERIVED adjoint equation
\ (maki/spec.f EQ-ADJ@) run as an ordinary `equation` node whose operands are dO (the
\ cotangent) plus every OTHER forward factor, in the adjoint's factor order. A forward-only
\ equation (a gather, whose scatter-add adjoint the grammar cannot state) is the named
\ E-CAD-GRAD reject - never a wrong gradient. The compute-precision tag rides through
\ unchanged (host executor is exact; the tag selects the device MMA dtype at lowering only).
: BW-STEP-EQUATION ( CAD-KIND:node-id MIR:operand-ref -- )
   {: fn:CAD-KIND:node-id ct:MIR:operand-ref :}
   fn MIR-ATTR@ {: attr:n :}
   attr CPREC-PAYLOAD@ >EQ-SLOT {: s:eq-slot :}
   s EQ-DIFF? 0= if E-CAD-GRAD throw then
   attr CPREC@ {: cp:n :}
   fn MIR-IN-COUNT@ {: kk:n :}
   kk 0 ?do
      i {: gi:n :}                              \ factor gi receives this gradient
      fn gi MIR-INPUT-IDX MIR-IN@ {: dref:MIR:operand-ref :}
      s gi EQ-ADJ@ EQ-SLOT>N  cp CPREC-PACK {: eattr:n :}
      MAKI-OPKIND:EQUATION MIR-OP-BEGIN
      ct MIR-IN+                                \ operand 0 = dO (the cotangent)
      kk 0 ?do  i gi <> if  fn i MIR-INPUT-IDX MIR-IN@ MIR-IN+  then  loop   \ + every other factor
      dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  eattr  1  MIR-OP+ MIR-NODE-REF
      dref BW-ACCUM
   loop ;

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
      layernorm       OF BW-STEP-LAYERNORM ENDOF
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
      seg-attn        OF BW-STEP-SEGMENT-ATTN ENDOF
      seg-attn-bwd    OF E-BW-UNSUP throw  ENDOF
      equation        OF BW-STEP-EQUATION  ENDOF
      bcast-mul       OF BW-STEP-BCAST-MUL ENDOF
      dropout         OF BW-STEP-DROPOUT   ENDOF
      dropout-bwd     OF E-BW-UNSUP throw  ENDOF
      swiglu          OF BW-STEP-SWIGLU    ENDOF
      mha             OF BW-STEP-MHA       ENDOF
      mha-bwd         OF E-BW-UNSUP throw  ENDOF
   ;MATCH ;

\ ---- supported-op gate (usable BEFORE build to classify not-run) -------------
: BW-OK-OP? ( opkind -- bool )  dup ADJ-HAS?  swap ADJ-SUP?  and ;

public

\ first forward NODE lacking a supported adjoint, or -1 (scans forward nodes only);
\ returns the node index (the op family is refetched at the use site).
: BW-FIRST-BAD ( -- n )
   BW-BUILT? @ if BW-FWD-N @ else NODE-COUNT@ CAD-NUM:BW-IC>N then {: n:n :}
   n 0 ?do  i MIR-NODE-ID MIR-OP@ BW-OK-OP? 0= if i unloop exit then  loop  -1 ;

: BW-CAN? ( -- bool )  BW-FIRST-BAD 0< ;

private

: BW-RESET-TABLES ( -- )                       \ clear presence flags over the live (bound) ranges
   BW-CT-N @ 0 ?do  0 BW-CT-SET-P @ i T-AT !  loop
   BW-ISG-N @ 0 ?do  0 BW-ISG-SET-P @ i T-AT !  loop ;

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
   NODE-COUNT@ CAD-NUM:BW-IC>N 0= if E-BW-EMPTY throw then
   BW-CAN? 0= if
      BW-FIRST-BAD {: bad:n :}
      bad MIR-NODE-ID MIR-OP@ ADJ-HAS? if E-BW-UNSUP else E-BW-NOADJ then throw
   then
   NODE-COUNT@ CAD-NUM:BW-IC>N BW-CT-BIND         \ size the cotangent tables from the model (ceiling + bind)
   SLOT-COUNT@ CAD-NUM:BW-IC>N BW-ISG-BIND
   NODE-COUNT@ CAD-NUM:BW-IC>N BW-FWD-N !
   BW-RESET-TABLES
   BW-SEED-OUTPUT
   BW-FWD-N @ 0 ?do  BW-FWD-N @ 1- i - MIR-NODE-ID BW-STEP  loop
   -1 BW-BUILT? ! ;

\ ---- accessors --------------------------------------------------------------
: BW-FWD-N@     ( -- n )      BW-CK BW-FWD-N @ ;
: BW-BWD-COUNT  ( -- n )      BW-CK NODE-COUNT@ CAD-NUM:BW-IC>N BW-FWD-N @ - ;
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
   SB-RESET s" backward.seed: input " SB-APPEND BW-SEED @ SLOT>RAW FMT:SB-INT
   s"  (cotangent for node " SB-APPEND BW-FWD-N @ 1- FMT:SB-INT s" )" SB-APPEND SB$ ;

: BW-COUNT-ROW$ ( -- ptr u8 n )
   SB-RESET s" backward.nodes: fwd=" SB-APPEND BW-FWD-N @ FMT:SB-INT
   s"  bwd=" SB-APPEND NODE-COUNT@ CAD-NUM:BW-IC>N BW-FWD-N @ - FMT:SB-INT SB$ ;

: BW-GRAD-ROW$ ( MIR:input-slot -- ptr u8 n ) {: s:MIR:input-slot :}
   SB-RESET s" backward.grad: input " SB-APPEND s SLOT>RAW FMT:SB-INT
   s MIR-IN-REF {: ref:MIR:operand-ref :}
   ref BW-HAS? 0= if s"  none" SB-APPEND SB$ exit then
   ref BW-GET {: g:MIR:operand-ref :}
   g MIR-REF-INPUT?
   if s"  <- input " SB-APPEND g MIR-REF-SLOT SLOT>RAW FMT:SB-INT
   else s"  <- node " SB-APPEND g MIR-REF-NODE NODE>RAW FMT:SB-INT then  SB$ ;

: BW-INTO ( report -- report )
   BW-CK
   BW-SEED-ROW$ REPORT:WARN+
   BW-COUNT-ROW$ REPORT:WARN+
   SLOT-COUNT@ CAD-NUM:BW-IC>N 0 ?do
      i MIR-SLOT-ID {: s:MIR:input-slot :}
      s BW-SEED @ MIR-SLOT= 0= if s BW-GRAD-ROW$ REPORT:WARN+ then
   loop ;

;package
