\ profile.f - WPROF, the WebAssembly the Wasm backend generates code for: V1's
\ features, its call arity and its memory layout, as constants. The target
\ contract carries BASE and SCALAR-FP for wasm and nothing else
\ (src/compiler/target.f MASK-WASM), so these numbers live here.

package WPROF
public

\ The features past scalar core V1's code uses, as wasm-tools' --features list
\ spells them: multi-value for the (status, outputs) signature, and
\ saturating-float-to-int for `f>s`, whose i64.trunc_sat_f64_s truncates,
\ saturates and answers 0 for a NaN exactly as src/compiler/native/hir-word.f
\ defines realint.
: FEATURES ( -- ptr u8 n )
   s" multi-value,saturating-float-to-int" ;

\ The most input and output lanes a direct call passes; past either a function
\ takes the aligned frame (docs/wasm-backend.md section 7.3).
16 constant PARAMS-MAX
16 constant RESULTS-MAX

\ Section 17.5's layout: null [0,$10000), ctx [$10000,$11000), output
\ [$11000,$21000), data stack [$21000,$31000) - the native boot stack's 8192
\ cells - and static data from $31000. The slice has no allocator, so the
\ memory never grows.
$10000 constant CTX-BASE
$11000 constant OUT-BASE
$21000 constant STACK-BASE
$31000 constant DATA-BASE

\ Each context field's byte offset inside the context. Each has its own
\ eight-byte slot, so the i64 throw code and fault address and the i32 fields
\ beside them are all naturally aligned.
0 constant CTX-STACK-BASE
8 constant CTX-STACK-TOP
16 constant CTX-OUT-LEN
24 constant CTX-THROW-CODE
32 constant CTX-FAULT-KIND
40 constant CTX-FAULT-ADDR

;package
