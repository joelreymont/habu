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
\ Fail closed (NEVER a silent partial gradient): an op with no adjoint (cast) or a
\ v1-unsupported adjoint (slice/gather/bias/scale/linear, maki/adjoint.f ADJ-SUP?) is a
\ named throw BEFORE any node is appended; an empty IR and an accessor used before
\ BW-BUILD are named throws. maki -> habu only; backward owns -5105..-5109.

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
: REF-DT   ( n -- n ) {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-DT@   else r MIR-DT@   then ;
: REF-LAY  ( n -- n ) {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-LAY@  else r MIR-LAY@  then ;

\ ---- cotangent table access (node ref -> BW-CT, input ref -> BW-ISG) ----------
: BW-GET ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT cells BW-ISG + @ else ref cells BW-CT + @ then ;
: BW-SET ( n n -- ) {: ct:n ref:n :}
   ref MIR-REF-INPUT? if ct ref MIR-REF-SLOT cells BW-ISG + ! else ct ref cells BW-CT + ! then ;

\ ---- node emitters (append one backward node; mat flag is a placeholder FP-BUILD
\ overwrites via FP-MARK, so 1 = conservative "materialize" here) --------------
\ two operands, output descriptor taken from a reference tensor (the gradient's shape)
: BW-OP2 ( n n n n -- n ) {: a:n b:n op:n dref:n :}
   op MIR-OP-BEGIN  a MIR-IN+  b MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  0  1  MIR-OP+ ;

\ three operands, output descriptor from a reference tensor (rope: dz cos sin)
: BW-OP3 ( n n n n n -- n ) {: a:n b:n c:n op:n dref:n :}
   op MIR-OP-BEGIN  a MIR-IN+  b MIR-IN+  c MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  0  1  MIR-OP+ ;

\ matmul: out rows = a.rows, out cols = b.cols (the contraction result shape)
: BW-MM ( n n -- n ) {: a:n b:n :}
   OP-MATMUL MIR-OP-BEGIN  a MIR-IN+  b MIR-IN+
   a REF-ROWS  b REF-COLS  a REF-DT  LAY-ROW  0  1  MIR-OP+ ;

\ transpose: RxC -> CxR (movement node with staged verdict, like PLAN-TRANSPOSE)
: BW-TR ( n -- n ) {: s:n :}
   MV-TRANSPOSE MV-TRANSPOSE-VERDICT 0 0 MV-PACK {: attr:n :}
   OP-TRANSPOSE MIR-OP-BEGIN  s MIR-IN+
   s REF-COLS  s REF-ROWS  s REF-DT  s REF-LAY  attr  1  MIR-OP+ ;

\ reshape the cotangent to a target shape (movement node; verdict per target layout)
: BW-RS ( n n n n n -- n ) {: ct:n tr:n tc:n dt:n lay:n :}
   MV-RESHAPE  lay MV-RESHAPE-VERDICT  tr tc MV-PACK {: attr:n :}
   OP-RESHAPE MIR-OP-BEGIN  ct MIR-IN+
   tr tc dt lay  attr  1  MIR-OP+ ;

\ slice rows [r0,r1) of the cotangent; output descriptor from a reference tensor
: BW-SL ( n n n n -- n ) {: ct:n r0:n r1:n dref:n :}
   MV-SLICE  ct REF-LAY r0 dref REF-COLS MV-SLICE-VERDICT  r0 r1 MV-PACK {: attr:n :}
   OP-SLICE MIR-OP-BEGIN  ct MIR-IN+
   dref REF-ROWS dref REF-COLS dref REF-DT dref REF-LAY  attr  1  MIR-OP+ ;

\ ---- accumulate a new cotangent ref into a target operand (fan-out -> OP-ADD sum) --
: BW-ACCUM ( n n -- ) {: nc:n ref:n :}
   ref BW-GET {: cur:n :}
   cur BW-NONE = if nc ref BW-SET exit then
   cur nc OP-ADD ref BW-OP2  ref BW-SET ;

\ ---- per-adjoint emitters ( fn ct -- ) --------------------------------------
\ linear op: the cotangent copies unchanged to every masked input (add / residual)
: BW-STEP-COPY ( n n -- ) {: fn:n ct:n :}
   fn MIR-IN-COUNT@ 0 ?do
      fn MIR-OP@ i ADJ-GRAD-IN? if  ct  fn i MIR-IN@  BW-ACCUM  then
   loop ;

\ product rule: dx = ct*y, dy = ct*x (both operands saved)
: BW-STEP-MUL ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: y:n :}
   ct y OP-MUL x BW-OP2  x BW-ACCUM
   ct x OP-MUL y BW-OP2  y BW-ACCUM ;

\ dedicated elementwise / reduction backward op over (ct, saved-input)
: BW-STEP-UNARY ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}
   ct x  fn MIR-OP@ ADJ-BOP  x BW-OP2  x BW-ACCUM ;

\ softmax adjoint reads the saved OUTPUT row (the forward node itself)
: BW-STEP-SOFTMAX ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}
   ct fn OP-SOFTMAX-ROW-BWD x BW-OP2  x BW-ACCUM ;

\ rope adjoint rotates the cotangent by -angle, reading cos/sin (operands 1,2)
: BW-STEP-ROPE ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: c:n :}  fn 2 MIR-IN@ {: s:n :}
   ct c s OP-ROPE-BWD x BW-OP3  x BW-ACCUM ;

\ matmul adjoints are transposed matmuls: dX = ct @ Wt, dW = Xt @ ct
: BW-STEP-MATMUL ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}  fn 1 MIR-IN@ {: w:n :}
   ct  w BW-TR  BW-MM  x BW-ACCUM
   x BW-TR  ct  BW-MM  w BW-ACCUM ;

\ reshape adjoint reshapes the cotangent back to the input shape
: BW-STEP-RESHAPE ( n n -- ) {: fn:n ct:n :}
   fn 0 MIR-IN@ {: x:n :}
   ct  x REF-ROWS  x REF-COLS  x REF-DT  x REF-LAY  BW-RS  x BW-ACCUM ;

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

\ ---- one forward node's reverse step ---------------------------------------
: BW-STEP ( n -- ) {: fn:n :}
   fn cells BW-CT + @ {: ct:n :}
   ct BW-NONE = if exit then                 \ node not on the backward path
   fn ct  fn MIR-OP@ ADJ-ID case
      ADJ-COPY      of BW-STEP-COPY      endof
      ADJ-MUL       of BW-STEP-MUL       endof
      ADJ-RELU      of BW-STEP-UNARY     endof
      ADJ-GELU      of BW-STEP-UNARY     endof
      ADJ-SILU      of BW-STEP-UNARY     endof
      ADJ-LAYERNORM of BW-STEP-UNARY     endof
      ADJ-RMSNORM   of BW-STEP-UNARY     endof
      ADJ-SOFTMAX   of BW-STEP-SOFTMAX   endof
      ADJ-ROPE      of BW-STEP-ROPE      endof
      ADJ-MATMUL    of BW-STEP-MATMUL    endof
      ADJ-RESHAPE   of BW-STEP-RESHAPE   endof
      ADJ-TRANSPOSE of BW-STEP-TRANSPOSE endof
      ADJ-CONCAT    of BW-STEP-CONCAT    endof
      2drop E-BW-UNSUP throw
   endcase ;

\ ---- supported-op gate (usable BEFORE build to classify not-run) -------------
: BW-OK-OP? ( n -- bool ) {: op:n :}  op ADJ-HAS?  op ADJ-SUP?  and ;

public

\ first forward op-kind lacking a supported adjoint, or -1 (scans forward nodes only)
: BW-FIRST-BAD ( -- n )
   BW-BUILT? @ if BW-FWD-N @ else MIR-N@ then {: n:n :}
   n 0 ?do  i MIR-OP@ dup BW-OK-OP? 0= if unloop exit then drop  loop  -1 ;

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
      bad ADJ-HAS? if E-BW-UNSUP else E-BW-NOADJ then throw
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
   BW-SEED-ROW$ RPT-WARN+
   BW-COUNT-ROW$ RPT-WARN+
   MIR-IN-SLOTS@ 0 ?do
      i BW-SEED @ <> if  i BW-GRAD-ROW$ RPT-WARN+  then
   loop ;

end-package
