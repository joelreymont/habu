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
-100000 constant BW-NONE    \ "no cotangent yet" sentinel (no valid ref reaches it:
                            \ node refs are >= 0, input refs are in [-BW-SCAP, -1])

create BW-CT  BW-NCAP cells allot    \ per forward-node output cotangent ref
create BW-ISG BW-SCAP cells allot     \ per input-slot accumulated gradient ref
variable BW-FWD-N                      \ forward node count snapshot (backward appends past it)
variable BW-SEED                       \ seed cotangent input slot
variable BW-BUILT?

: BW-CK ( -- )  BW-BUILT? @ 0= if E-BW-STATE throw then ;

\ input-slot bounds check (backward.f owns no slot table; validate against model-ir)
: MIR-IS-CK-OK ( n -- n )  dup 0 < over MIR-IN-SLOTS@ >= or if E-BW-STATE throw then ;

\ ---- operand-ref descriptor (input slot or producer node) -------------------
: REF-ROWS ( n -- n ) {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-ROWS@ then ;
: REF-COLS ( n -- n ) {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-COLS@ then ;
: REF-DT   ( n -- dtype )  {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-DT@   else r MIR-DT@   then ;
: REF-LAY  ( n -- layout ) {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-LAY@  else r MIR-LAY@  then ;

\ ---- cotangent table access (node ref -> BW-CT, input ref -> BW-ISG) ----------
: BW-GET ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT cells BW-ISG + @ else ref cells BW-CT + @ then ;
: BW-SET ( n n -- ) {: ct:n ref:n :}
   ref MIR-REF-INPUT? if ct ref MIR-REF-SLOT cells BW-ISG + ! else ct ref cells BW-CT + ! then ;

\ ---- node emitters (append one backward node; mat flag is a placeholder FP-BUILD
\ overwrites via FP-MARK, so 1 = conservative "materialize" here) --------------
\ two operands, output descriptor taken from a reference tensor (the gradient's shape)
\ the op family cannot bind into a local, so it is consumed by MIR-OP-BEGIN off the
\ top (after dref binds) before the operand refs bind.
: BW-OP2 ( n n opkind n -- n ) {: dref:n :}
   MIR-OP-BEGIN {: a:n b:n :}
   a MIR-IN+  b MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  0  1  MIR-OP+ ;

\ three operands, output descriptor from a reference tensor (rope: dz cos sin)
: BW-OP3 ( n n n opkind n -- n ) {: dref:n :}
   MIR-OP-BEGIN {: a:n b:n c:n :}
   a MIR-IN+  b MIR-IN+  c MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  0  1  MIR-OP+ ;

\ matmul: out rows = a.rows, out cols = b.cols (the contraction result shape)
: BW-MM ( n n -- n ) {: a:n b:n :}
   MAKI-OPKIND:MATMUL MIR-OP-BEGIN  a MIR-IN+  b MIR-IN+
   a REF-ROWS  b REF-COLS  a REF-DT  MAKI-LAYOUT:ROW  0  1  MIR-OP+ ;

\ transpose: RxC -> CxR (movement node with staged verdict, like PLAN-TRANSPOSE)
: BW-TR ( n -- n ) {: s:n :}
   MV-TRANSPOSE MV-TRANSPOSE-VERDICT 0 0 MV-PACK {: attr:n :}
   MAKI-OPKIND:TRANSPOSE MIR-OP-BEGIN  s MIR-IN+
   s REF-COLS  s REF-ROWS  s REF-DT  s REF-LAY  attr  1  MIR-OP+ ;

\ reshape the cotangent to a target shape (movement node; verdict per target
\ layout); dtype/layout come from a reference tensor like the other emitters,
\ so the families ride the stack straight into MIR-OP+
: BW-RS ( n n n n -- n ) {: ct:n tr:n tc:n dref:n :}   \ ct target-rows target-cols dref
   MV-RESHAPE  dref REF-LAY MV-RESHAPE-VERDICT  tr tc MV-PACK {: attr:n :}
   MAKI-OPKIND:RESHAPE MIR-OP-BEGIN  ct MIR-IN+
   tr tc  dref REF-DT  dref REF-LAY  attr  1  MIR-OP+ ;

\ slice rows [r0,r1) of the cotangent; output descriptor from a reference tensor
: BW-SL ( n n n n -- n ) {: ct:n r0:n r1:n dref:n :}
   MV-SLICE  ct REF-LAY r0 dref REF-COLS MV-SLICE-VERDICT  r0 r1 MV-PACK {: attr:n :}
   MAKI-OPKIND:SLICE MIR-OP-BEGIN  ct MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  attr  1  MIR-OP+ ;

\ row-reduce the cotangent over its rows -> 1 x C (the bias / linear-bias gradient);
\ output cols/dtype/layout come from the bias reference tensor (1xC).
: BW-ROWSUM ( n n -- n ) {: ct:n bref:n :}
   MAKI-OPKIND:ROWSUM-BWD MIR-OP-BEGIN  ct MIR-IN+
   1 bref REF-COLS  bref REF-DT  bref REF-LAY  0  1  MIR-OP+ ;

\ full-reduce dot of the cotangent with the saved input -> 1 x 1 (the scale gradient);
\ output dtype/layout come from the scalar (scale-factor) reference tensor.
: BW-FULLSUM ( n n n -- n ) {: ct:n x:n sref:n :}
   MAKI-OPKIND:FULLSUM-DOT-BWD MIR-OP-BEGIN  ct MIR-IN+  x MIR-IN+
   1 1  sref REF-DT  sref REF-LAY  0  1  MIR-OP+ ;

\ pad-scatter the cotangent into a zero R x C buffer at row r0 (the slice input-grad);
\ output extents from the slice's forward input, r0/r1 packed like the forward slice.
: BW-PS ( n n n n -- n ) {: ct:n r0:n r1:n dref:n :}
   MV-SLICE MVV-MATERIALIZE r0 r1 MV-PACK {: attr:n :}
   MAKI-OPKIND:PAD-SCATTER MIR-OP-BEGIN  ct MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  attr  1  MIR-OP+ ;

\ scatter-add the cotangent rows into a zero R x C buffer at the gathered indices (the
\ gather input-grad); reads the index operand, output extents from the gather's input.
: BW-SA ( n n n -- n ) {: ct:n idx:n dref:n :}
   MV-GATHER MVV-MATERIALIZE 0 0 MV-PACK {: attr:n :}
   MAKI-OPKIND:SCATTER-ADD MIR-OP-BEGIN  ct MIR-IN+  idx MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  attr  1  MIR-OP+ ;

\ ---- accumulate a new cotangent ref into a target operand (fan-out -> OP-ADD sum) --
: BW-ACCUM ( n n -- ) {: nc:n ref:n :}
   ref BW-GET {: cur:n :}
   cur BW-NONE = if nc ref BW-SET exit then
   cur nc MAKI-OPKIND:ADD ref BW-OP2  ref BW-SET ;

\ ---- per-adjoint emitters ( fn ct -- ) --------------------------------------
\ linear op: the cotangent copies unchanged to every masked input (add / residual)
: BW-STEP-COPY ( n n -- ) {: fn:n ct:n :}
   fn MIR-IN-COUNT@ 0 ?do
      fn MIR-OP@ i ADJ-GRAD-IN? if  ct  fn i MIR-IN@  BW-ACCUM  then
   loop ;

\ product rule: dx = ct*y, dy = ct*x (both operands saved)
: BW-STEP-MUL ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: y:n :}
   ct y MAKI-OPKIND:MUL x BW-OP2  x BW-ACCUM
   ct x MAKI-OPKIND:MUL y BW-OP2  y BW-ACCUM ;

\ dedicated elementwise / reduction backward op over (ct, saved-input)
: BW-STEP-UNARY ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}
   ct x  fn MIR-OP@ ADJ-BWD-KIND  x BW-OP2  x BW-ACCUM ;

\ softmax adjoint reads the saved OUTPUT row (the forward node itself)
: BW-STEP-SOFTMAX ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}
   ct fn MAKI-OPKIND:SOFTMAX-ROW-BWD x BW-OP2  x BW-ACCUM ;

\ rope adjoint rotates the cotangent by -angle, reading cos/sin (operands 1,2)
: BW-STEP-ROPE ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: c:n :}  fn 2 MIR-IN@ {: s:n :}
   ct c s MAKI-OPKIND:ROPE-BWD x BW-OP3  x BW-ACCUM ;

\ matmul adjoints are transposed matmuls: dX = ct @ Wt, dW = Xt @ ct
: BW-STEP-MATMUL ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: w:n :}
   ct  w BW-TR  BW-MM  x BW-ACCUM
   x BW-TR  ct  BW-MM  w BW-ACCUM ;

\ reshape adjoint reshapes the cotangent back to the input shape
: BW-STEP-RESHAPE ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}
   ct  x REF-ROWS  x REF-COLS  x  BW-RS  x BW-ACCUM ;

\ transpose adjoint transposes the cotangent (self-inverse)
: BW-STEP-TRANSPOSE ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}
   ct BW-TR  x BW-ACCUM ;

\ concat adjoint splits the cotangent into one slice per input (row ranges)
: BW-STEP-CONCAT ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: a:n :}  fn 1 MIR-IN@ {: b:n :}
   a REF-ROWS {: ra:n :}  b REF-ROWS {: rb:n :}
   ct 0 ra a BW-SL  a BW-ACCUM
   ct ra  ra rb +  b BW-SL  b BW-ACCUM ;

\ bias adjoint: dx = ct (copy) ; d-bias = row-reduce of ct over its broadcast rows -> 1xC
: BW-STEP-BIAS ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: b:n :}
   ct x BW-ACCUM                              \ dx = cotangent copy
   ct b BW-ROWSUM  b BW-ACCUM ;               \ d-bias = rowsum(ct) -> 1 x C

\ scale adjoint: dx = ct*s, d-scale depends on the scale-operand shape. A same-shape
\ operand is the elementwise product rule (OP-MUL both); a 1x1 scalar is broadcast-scale
\ (dx = OP-SCALE(ct,s), d-scale = OP-FULLSUM-DOT-BWD(ct,x) -> 1x1); a partial broadcast
\ (1xC / Rx1) needs a broadcast-reduce not in v1 -> fail closed (E-BW-BROADCAST).
: BW-STEP-SCALE ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: s:n :}
   s REF-ROWS x REF-ROWS =  s REF-COLS x REF-COLS =  and if
      ct s MAKI-OPKIND:MUL x BW-OP2  x BW-ACCUM        \ dx = ct*s (elementwise)
      ct x MAKI-OPKIND:MUL s BW-OP2  s BW-ACCUM        \ d-scale = ct*x (elementwise)
      exit
   then
   s REF-ROWS 1 =  s REF-COLS 1 =  and 0= if E-BW-BROADCAST throw then
   ct s MAKI-OPKIND:SCALE x BW-OP2  x BW-ACCUM         \ dx = scale(ct, s) (broadcast-by-1x1)
   ct x s BW-FULLSUM  s BW-ACCUM ;            \ d-scale = fullsum-dot(ct, x) -> 1x1

\ linear adjoint: the matmul adjoints for x and w plus the bias row-reduce.
\ dX = ct @ Wt, dW = Xt @ ct, dB = rowsum(ct)
: BW-STEP-LINEAR ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: w:n :}  fn 2 MIR-IN@ {: b:n :}
   ct  w BW-TR  BW-MM  x BW-ACCUM             \ dX = ct @ Wt
   x BW-TR  ct  BW-MM  w BW-ACCUM             \ dW = Xt @ ct
   ct b BW-ROWSUM  b BW-ACCUM ;               \ dB = rowsum(ct) -> 1 x N

\ slice adjoint: pad-scatter the cotangent into a zero buffer at the forward slice offset
\ (r0/r1 read back from the forward node's packed attrs).
: BW-STEP-SLICE ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn MIR-ATTR@ {: attr:n :}
   ct  attr MV-PA@  attr MV-PB@  x  BW-PS  x BW-ACCUM ;

\ gather adjoint: scatter-add the cotangent rows back to the gathered indices (operand 1);
\ duplicates accumulate. The index operand carries no gradient (mask bit 1 = 0).
: BW-STEP-GATHER ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: idx:n :}
   ct idx x  BW-SA  x BW-ACCUM ;

\ gelu-bwd adjoint (second order): the node computes z = dz * gelu'(x) over (dz, x).
\ d-dz = ct * gelu'(x)      = OP-GELU-BWD(ct, x)        (reuses the first derivative)
\ d-x  = ct * dz * gelu''(x) = OP-GELU-BWD2(ct*dz, x)   (needs the second derivative)
: BW-STEP-GELU-BWD ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: dz:n :}  fn 1 MIR-IN@ {: x:n :}
   ct x MAKI-OPKIND:GELU-BWD dz BW-OP2  dz BW-ACCUM
   ct dz MAKI-OPKIND:MUL x BW-OP2
   x MAKI-OPKIND:GELU-BWD2 x BW-OP2  x BW-ACCUM ;

\ ---- one forward node's reverse step ---------------------------------------
: BW-STEP ( n -- ) {: fn:n :}
   fn cells BW-CT + @ {: ct:n :}
   ct BW-NONE = if exit then                 \ node not on the backward path
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
   n 0 ?do  i MIR-OP@ BW-OK-OP? 0= if i unloop exit then  loop  -1 ;

: BW-CAN? ( -- bool )  BW-FIRST-BAD 0< ;

private

: BW-RESET-TABLES ( -- )
   BW-NCAP 0 ?do  BW-NONE i cells BW-CT + !  loop
   BW-SCAP 0 ?do  BW-NONE i cells BW-ISG + !  loop ;

\ seed the output node (last forward node) with a fresh cotangent input slot
: BW-SEED-OUTPUT ( -- )
   BW-FWD-N @ 1- {: out:n :}
   out MIR-ROWS@ out MIR-COLS@ out MIR-DT@ out MIR-LAY@ MIR-INPUT+ {: sslot:n :}
   sslot BW-SEED !
   sslot MIR-IN-REF  out cells BW-CT + ! ;

public

\ ---- build the backward IR over the current forward node table ---------------
: BW-BUILD ( -- )
   0 BW-BUILT? !
   MIR-N@ 0= if E-BW-EMPTY throw then
   MIR-N@ BW-NCAP > MIR-IN-SLOTS@ BW-SCAP >= or if E-BW-CAP throw then
   BW-CAN? 0= if
      BW-FIRST-BAD {: bad:n :}
      bad MIR-OP@ ADJ-HAS? if E-BW-UNSUP else E-BW-NOADJ then throw
   then
   MIR-N@ BW-FWD-N !
   BW-RESET-TABLES
   BW-SEED-OUTPUT
   BW-FWD-N @ 0 ?do  BW-FWD-N @ 1- i -  BW-STEP  loop
   -1 BW-BUILT? ! ;

\ ---- accessors --------------------------------------------------------------
: BW-FWD-N@     ( -- n )      BW-CK BW-FWD-N @ ;
: BW-BWD-COUNT  ( -- n )      BW-CK MIR-N@ BW-FWD-N @ - ;
: BW-SEED-SLOT@ ( -- n )      BW-CK BW-SEED @ ;
: BW-NODE-CT@   ( n -- n )    BW-CK  dup 0 < over BW-FWD-N @ >= or if E-BW-STATE throw then  cells BW-CT + @ ;
: BW-SLOT-GRAD@ ( n -- n )    BW-CK  MIR-IS-CK-OK  cells BW-ISG + @ ;
: BW-HAS-GRAD?  ( n -- bool )  BW-SLOT-GRAD@ BW-NONE <> ;   \ input slot received a gradient

\ ---- report: the seed, the backward node count, and each model input's gradient --
: BW-SEED-ROW$ ( -- ptr u8 n )
   SB-RESET s" backward.seed: input " SB-APPEND BW-SEED @ SB-INT
   s"  (cotangent for node " SB-APPEND BW-FWD-N @ 1- SB-INT s" )" SB-APPEND SB$ ;

: BW-COUNT-ROW$ ( -- ptr u8 n )
   SB-RESET s" backward.nodes: fwd=" SB-APPEND BW-FWD-N @ SB-INT
   s"  bwd=" SB-APPEND MIR-N@ BW-FWD-N @ - SB-INT SB$ ;

: BW-GRAD-ROW$ ( n -- ptr u8 n ) {: s:n :}
   SB-RESET s" backward.grad: input " SB-APPEND s SB-INT
   s cells BW-ISG + @ {: g:n :}
   g BW-NONE = if s"  none" SB-APPEND
   else g MIR-REF-INPUT? if s"  <- input " SB-APPEND g MIR-REF-SLOT SB-INT
   else s"  <- node " SB-APPEND g SB-INT then then  SB$ ;

: BW-INTO ( report -- report )
   BW-CK
   BW-SEED-ROW$ REPORT:WARN+
   BW-COUNT-ROW$ REPORT:WARN+
   MIR-IN-SLOTS@ 0 ?do
      i BW-SEED @ <> if  i BW-GRAD-ROW$ REPORT:WARN+  then
   loop ;

end-package
