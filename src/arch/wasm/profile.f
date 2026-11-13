\ profile.f - WPROF, the Wasm V1 the backend targets: features, call arity and
\ memory layout. The target contract carries only BASE and SCALAR-FP for wasm
\ (src/compiler/target.f MASK-WASM), so these numbers live here. A Wasm build
\ loads it as a start-up row before `require` exists, after
\ src/habu/stack-abi.f (src/habu/boot-files.f), so it requires nothing.

package WPROF
public

\ The features past scalar core V1's code uses, as wasm-tools' --features list
\ spells them: multi-value for the (status, outputs) signature, and
\ saturating-float-to-int for `f>s`, whose i64.trunc_sat_f64_s truncates,
\ saturates and answers 0 for a NaN, and bulk-memory for clearing reused page runs.
: FEATURES ( -- ptr u8 n )
   s" multi-value,saturating-float-to-int,bulk-memory" ;

\ The most input and output lanes a direct call passes; past either a function
\ takes the aligned frame.
16 constant PARAMS-MAX
16 constant RESULTS-MAX

$10000 constant PAGE-BYTES
$10000 constant PAGES-MAX

\ Page 0 is null. Dynamic mappings lie past the memory's last page, in pages
\ map-anon grows.
$10000 constant CTX-BASE
$11000 constant OUT-BASE
$21000 constant STACK-BASE
$10000 constant STACK-BYTES
STACK-BASE STACK-BYTES + constant STACK-END
STACK-BASE OUT-BASE - constant OUT-CAP        \ the OUT region's bytes, all a run writes

\ The return stack and the DO frames follow the data stack, each its STACK-ABI
\ size; a push checks its region's end, since Wasm memory has no guard page.
STACK-END constant RETURN-BASE
RETURN-BASE STACK-ABI:RETURN-BYTES + constant LOOP-BASE

\ The DATA region holds a build's data space from DATA-BASE, offset for offset,
\ header band included: a Wasm target's DATA-VA and DATA-SIZE are these two.
LOOP-BASE STACK-ABI:LOOP-BYTES + constant DATA-BASE
PAGES-MAX PAGE-BYTES * DATA-BASE - constant DATA-MAX

\ One eight-byte slot per field, so i64 and i32 fields are all naturally aligned.
0 constant CTX-STACK-BASE
8 constant CTX-STACK-TOP
16 constant CTX-OUT-LEN
24 constant CTX-THROW-CODE
32 constant CTX-FAULT-KIND
40 constant CTX-FAULT-ADDR
48 constant CTX-FREE-HEAD                 \ the first free page run, 0 for none; runs keep it
56 constant CTX-ENV                       \ the record DISPATCH, leaves for a reading quotation's entry

;package
