\ perf-watch.f - canonical PTX-producer watch table for the kernel-perf lint.
\
\ ONE concern: the exact set of source paths whose content reaches lowered PTX
\ (and therefore GEMM/kernel performance), so a diff that touches one demands a
\ profile/waiver row (tools/kernel-perf-lint-core.f) and a waiver may only own
\ one (tools/ptx/perf-registry.f). Membership is EXACT-PATH, never a directory
\ prefix or a suffix rule: a table entry names one real file, so every entry can
\ be resolved on disk and a near-miss / test path (lib/ptx/cg-collective-test.f,
\ lib/ptx/tile-test.f) never matches by accident.
\
\ Coverage is the full producer pipeline: the PTX text encoder + VJP table
\ (src/arch/ptx), the shared lib/ptx machinery (expression IR, the tile DSL, the
\ bit-exact optimizer, the emit-mode codegen + autodiff lowering, the
\ ABI/header/collective/register-emitter vocabularies), and the per-kernel emit
\ drivers (tools/ptx/*-cg.f) that real waivers own. NON-PRODUCER holds the
\ non-emitting lib/ptx neighbours (runtime driver, launch contract, host AD
\ evaluator, export-manifest renderer, device sentinel, assembler runner, test
\ support) and the one deliberately-wrong driver fixture, each with a reason, so
\ every file in the scanned dirs is classified. CLASSIFY drives the completeness
\ ratchet (kernel-perf-lint-test.f): a producer added to lib/ptx / src/arch/ptx /
\ tools/ptx without a WATCH row is PW-UNKNOWN and fails the gate mechanically.
\
\ A path-set (PS-*) is a checked, deduped, capacity-bounded byte arena: PS-ADD
\ rejects a duplicate (E-WATCH-DUP) or an overflow (E-WATCH-CAP) at build, so the
\ committed table is duplicate-free by construction and the same primitive backs
\ the ratchet's manifest walk. Load after lib/errors.f, lib/string.f, lib/fs.f.

require lib/errors.f
require lib/string.f
require lib/fs.f

-7320 constant E-WATCH-DUP    \ a path added to a watch set twice
-7321 constant E-WATCH-CAP    \ a watch set exceeded its path or arena capacity

package PERF-WATCH
private

192 constant PS-MAX           \ max paths in one set (WATCH 57, ratchet manifest ~113)
$2000 constant PS-ARENA       \ per-set path-byte arena

\ A set is TWO blocks, because it holds two different kinds of thing and a
\ pointer has one element type. The INDEX block is cells - the arena bump, the
\ live count, and the per-entry offset and length rows - so it is a `ptr n`. The
\ ARENA block is the packed path bytes, so it is a `ptr u8`. They used to be one
\ `ptr a` block, which forced every accessor to read cells and hand back a byte
\ span through the same pointer; that only certified while a pointee still
\ admitted one integer type for another.
0                      constant PS.USED     \ index cell: bytes used in the arena
1 cells                constant PS.CNT      \ index cell: live path count
2 cells                constant PS.OFFS     \ index rows: arena offset per entry
PS.OFFS PS-MAX cells + constant PS.LENS     \ index rows: byte length per entry
PS.LENS PS-MAX cells + constant PS-INDEX-SIZE
PS-ARENA               constant PS-ARENA-SIZE

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

public

\ Every entry point takes the set as its two blocks, in the same order: the
\ index cells first, then the path arena.
: INDEX-BYTES ( -- n ) PS-INDEX-SIZE ;
: ARENA-BYTES ( -- n ) PS-ARENA-SIZE ;

: PS-RESET ( ptr n -- ) {: ix:ptr :}
   0 ix PS.USED + !   0 ix PS.CNT + ! ;

: PS-N ( ptr n -- n )
   PS.CNT + @ ;

: PS-AT ( ptr n ptr u8 n -- ptr u8 n ) {: ix:ptr ar:ptr i:n :}
   ar  ix PS.OFFS + i cells + @ +
   ix PS.LENS + i cells + @ ;

: PS-HAS? ( ptr n ptr u8 ptr u8 n -- bool ) {: ix:ptr ar:ptr a:ptr u:n :}
   ix PS-N 0 ?do
      ix ar i PS-AT a u STR= if true unloop exit then
   loop false ;

: PS-ADD ( ptr n ptr u8 ptr u8 n -- ) {: ix:ptr ar:ptr a:ptr u:n :}
   ix ar a u PS-HAS? if E-WATCH-DUP throw then
   ix PS-N PS-MAX < 0= if E-WATCH-CAP throw then
   ix PS.USED + @ u + PS-ARENA > if E-WATCH-CAP throw then
   a  ar ix PS.USED + @ +  u BYTE-COPY
   ix PS.USED + @   ix PS.OFFS + ix PS-N cells + !
   u               ix PS.LENS + ix PS-N cells + !
   ix PS.USED + @ u +  ix PS.USED + !
   ix PS-N 1+ ix PS.CNT + ! ;

private

create WATCH-IX   PS-INDEX-SIZE allot
create WATCH-AR   PS-ARENA-SIZE allot
create EXCLUDE-IX PS-INDEX-SIZE allot
create EXCLUDE-AR PS-ARENA-SIZE allot

: +P ( ptr u8 n -- ) {: a:ptr u:n :}   \ add one canonical producer path
   WATCH-IX WATCH-AR a u PS-ADD ;

: +X ( ptr u8 n -- ) {: a:ptr u:n :}   \ add one non-producer neighbour path
   WATCH-IX WATCH-AR a u PS-HAS? if E-WATCH-DUP throw then
   EXCLUDE-IX EXCLUDE-AR a u PS-ADD ;

: BUILD-WATCH ( -- )
   WATCH-IX PS-RESET
   \ --- PTX text encoder + VJP table (src/arch/ptx) ---
   s" src/arch/ptx/emit.f" +P
   s" src/arch/ptx/vjp.f" +P
   \ --- expression IR (lib/ptx) ---
   s" lib/ptx/ir.f" +P
   \ --- tile DSL vocabulary (lib/ptx) ---
   s" lib/ptx/tile.f" +P
   s" lib/ptx/tile-loop.f" +P
   s" lib/ptx/tile-smem.f" +P
   s" lib/ptx/tile-acc.f" +P
   s" lib/ptx/tile-v4.f" +P
   s" lib/ptx/tile-v4a.f" +P
   s" lib/ptx/tile-pipe.f" +P
   s" lib/ptx/collective.f" +P
   s" lib/ptx/cpp-slot.f" +P
   \ --- register-emitter / ABI / header vocabulary (lib/ptx) ---
   s" lib/ptx/rep.f" +P
   s" lib/ptx/header.f" +P
   s" lib/ptx/kernel-abi.f" +P
   \ --- emit-mode codegen (lib/ptx) ---
   s" lib/ptx/cg.f" +P
   s" lib/ptx/cg-vec.f" +P
   s" lib/ptx/cg-collective.f" +P
   s" lib/ptx/cg-activation.f" +P
   s" lib/ptx/cg-attention.f" +P
   s" lib/ptx/cg-matmul.f" +P
   s" lib/ptx/cg-matmul-emit.f" +P
   s" lib/ptx/cg-matmul-naive.f" +P
   s" lib/ptx/cg-mma.f" +P
   \ --- autodiff transform / lowering (lib/ptx) ---
   s" lib/ptx/ad.f" +P
   s" lib/ptx/ad-dag.f" +P
   s" lib/ptx/ad-gen.f" +P
   s" lib/ptx/ad-ir.f" +P
   s" lib/ptx/ad-saved.f" +P
   \ --- per-kernel emit drivers (tools/ptx/*-cg.f) ---
   s" tools/ptx/saxpy-cg.f" +P
   s" tools/ptx/saxpy-v4-cg.f" +P
   s" tools/ptx/relu-cg.f" +P
   s" tools/ptx/relu-v4-cg.f" +P
   s" tools/ptx/fused-relu-cg.f" +P
   s" tools/ptx/exp-cg.f" +P
   s" tools/ptx/expbwd-cg.f" +P
   s" tools/ptx/maxselect-cg.f" +P
   s" tools/ptx/matmul-cg.f" +P
   s" tools/ptx/attention-cg.f" +P
   s" tools/ptx/smem-cg.f" +P
   s" tools/ptx/ops-cg.f" +P
   s" tools/ptx/acc-cg.f" +P
   s" tools/ptx/once-cg.f" +P
   s" tools/ptx/redadd-cg.f" +P
   s" tools/ptx/scatter-add-grad-cg.f" +P
   s" tools/ptx/indexed-scatter-cg.f" +P
   s" tools/ptx/sum-cg.f" +P
   s" tools/ptx/sum1024-cg.f" +P
   s" tools/ptx/softmax-cg.f" +P
   s" tools/ptx/softmax-bwd-cg.f" +P
   s" tools/ptx/softmax-bwd-opt-cg.f" +P
   s" tools/ptx/softmax-fb-cg.f" +P
   s" tools/ptx/softmax-rows-bwd-cg.f" +P
   s" tools/ptx/rmsnorm-cg.f" +P
   s" tools/ptx/layernorm-cg.f" +P
   s" tools/ptx/rope-cg.f" +P
   s" tools/ptx/swiglu-cg.f" +P ;

: BUILD-EXCLUDE ( -- )
   EXCLUDE-IX PS-RESET
   s" lib/ptx/cuda-driver.f" +X          \ CUDA Driver API runtime bindings, not codegen
   s" lib/ptx/launch.f" +X               \ launch-contract validation, emits no PTX
   s" lib/ptx/sentinel.f" +X             \ device-readback poison fill, host-side, no PTX
   s" lib/ptx/toolchain.f" +X            \ ptxas assembler runner: consumes PTX, emits cubin
   s" lib/ptx/kernel-manifest.f" +X      \ export-manifest JSON renderer, hashes PTX, no PTX text
   s" lib/ptx/ad-dag-eval.f" +X          \ host numeric AD evaluator, no PTX emission
   s" lib/ptx/neg-test-lib.f" +X         \ require-only rejection-test helper
   s" lib/ptx/test-prelude.f" +X         \ test-support prelude
   s" lib/ptx/process-test-prelude.f" +X \ test-support prelude
   s" lib/ptx/cuda-scope.f" +X           \ exception-safe CUDA resource ownership, runtime, no PTX
   s" tools/ptx/saxpy-wrong-cg.f" +X ;   \ deliberately-wrong driver fixture, not perf-tracked

public

0 constant PW-WATCHED     \ path is a canonical producer (WATCH)
1 constant PW-EXCLUDED    \ path is a declared non-producer neighbour (EXCLUDE)
2 constant PW-TEST        \ path is a *-test.f source (skipped by the ratchet)
3 constant PW-UNKNOWN     \ path is an unclassified producer (ratchet failure)

: PRODUCER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   WATCH-IX WATCH-AR a u PS-HAS? ;

: NON-PRODUCER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   EXCLUDE-IX EXCLUDE-AR a u PS-HAS? ;

: TEST-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ a *-test.f (covers -neg-test.f)
   a u s" -test.f" ENDS-WITH? ;

: CLASSIFY ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u PRODUCER? if PW-WATCHED exit then
   a u TEST-PATH? if PW-TEST exit then
   a u NON-PRODUCER? if PW-EXCLUDED exit then
   PW-UNKNOWN ;

\ The two blocks of each set, for a caller that walks it with PS-AT.
: WATCH-INDEX ( -- ptr n ) WATCH-IX ;
: WATCH-ARENA ( -- ptr u8 ) WATCH-AR ;
: EXCLUDE-INDEX ( -- ptr n ) EXCLUDE-IX ;
: EXCLUDE-ARENA ( -- ptr u8 ) EXCLUDE-AR ;

: #PRODUCERS ( -- n ) WATCH-IX PS-N ;
: PRODUCER-PATH$ ( n -- ptr u8 n ) {: i:n :} WATCH-IX WATCH-AR i PS-AT ;

private

: SELF-CHECK ( -- )   \ no *-test.f may be listed as a producer (else CLASSIFY masks it)
   0 begin dup #PRODUCERS < while
      dup PRODUCER-PATH$ TEST-PATH? if E-WATCH-DUP throw then
      1+
   repeat drop ;

BUILD-WATCH
BUILD-EXCLUDE
SELF-CHECK

;package
