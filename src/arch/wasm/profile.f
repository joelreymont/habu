\ profile.f - WPROF, the Wasm V1 the backend targets: features, call arity and
\ memory layout. The target contract carries only BASE and SCALAR-FP for wasm
\ (src/compiler/target.f MASK-WASM), so these numbers live here.

require src/habu/layout.f

package WPROF
public

\ The features past scalar core V1's code uses, as wasm-tools' --features list
\ spells them: multi-value for the (status, outputs) signature, and
\ saturating-float-to-int for `f>s`, whose i64.trunc_sat_f64_s truncates,
\ saturates and answers 0 for a NaN exactly as src/compiler/native/hir-word.f
\ defines realint, and bulk-memory for clearing reused page runs.
: FEATURES ( -- ptr u8 n )
   s" multi-value,saturating-float-to-int,bulk-memory" ;

\ The most input and output lanes a direct call passes; past either a function
\ takes the aligned frame (docs/xcomp.md "Calls").
16 constant PARAMS-MAX
16 constant RESULTS-MAX

$10000 constant PAGE-BYTES
$10000 constant PAGES-MAX

\ Page 0 is null. The DATA region is the single-task DATA ABI region and covers
\ the shared layout's reserved band through DATA-START. Dynamic mappings lie past
\ the image's last page, in pages map-anon grows.
$10000 constant CTX-BASE
$11000 constant OUT-BASE
$21000 constant STACK-BASE
$31000 constant DATA-BASE
DATA-BASE STACK-BASE - constant STACK-BYTES
DATA-BASE constant STACK-END
DATA-START PAGE-BYTES 1- + PAGE-BYTES / PAGE-BYTES * constant DATA-BYTES
DATA-BASE DATA-BYTES + constant IMAGE-BASE

\ One eight-byte slot per field, so i64 and i32 fields are all naturally aligned.
0 constant CTX-STACK-BASE
8 constant CTX-STACK-TOP
16 constant CTX-OUT-LEN
24 constant CTX-THROW-CODE
32 constant CTX-FAULT-KIND
40 constant CTX-FAULT-ADDR
48 constant CTX-FREE-HEAD                 \ the first free page run, 0 for none; runs keep it

;package
