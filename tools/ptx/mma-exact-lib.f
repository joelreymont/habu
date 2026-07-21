\ mma-exact-lib.f - import-safe MMA element-exact validation library (package MMA-EXACT).
\
\ The canonical integer-exact MMA proof machinery, owned in ONE place and shared by
\ tools/ptx/mma-gemm-check.f (the device-correctness campaign) and tools/ptx/autotune-sweep.f
\ (the GB sweep's element-exact precondition). Before this library both files carried their
\ own seven-buffer fill/reference/compare/pack copy, because mma-gemm-check ran its device
\ campaign at load and could not be imported; the sweep duplicated the proof to reuse it.
\
\ IMPORT-SAFE by construction: loading this file allocates NO host or device memory, opens
\ NO CUDA context, assembles nothing, spawns nothing, prints nothing, and runs no campaign.
\ The seven large host/packed buffers are HEAP-allocated only when MX-BUF-INIT is called
\ (idempotent), so a bare `require` leaves every base pointer 0 and every device handle 0.
\ The device primitives (alloc/htod/params/launch-independent validation) only touch the
\ GPU when an explicit command invokes them. The verification of that invariant, plus the
\ launch-independent exactness math, is tools/ptx/mma-exact-lib-test.f.
\
\ Element-EXACT design (as in mma-probe / mma-gemm-check): small integer A,B entries so every
\ value is exact in the tensor dtype's mantissa and every f32 accumulation stays < 2^24, so a
\ correct kernel reproduces the integer matmul BIT-EXACT (zero tolerance, no epsilon). Varied
\ (non-constant) entries so a store mis-mapping mismatches instead of hiding behind a symmetric
\ tile. The buffers are sized for the 512^3 (XSWIZ / grouped-raster) checks.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/memory.f
require maki/array.f
require lib/fs.f
require lib/fs-mutate.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/cg-matmul.f
require lib/ptx/cg-mma.f
require lib/ptx/toolchain.f
require maki/eval/active-target.f
require tools/ptx/bench.f

package MMA-EXACT
public

512 constant MX-MAX                        \ largest square edge (buffers sized for this; 512 = grouped-raster / XSWIZ 512^3 checks)
MX-MAX MX-MAX * constant MX-CAP            \ 262144 elems at 512^2
-6101 constant MX-E-ZEROBLK                \ launch M < the block rows (BROWS) or a ragged multiple -> silent zero/partial C
-6110 constant MX-E-CAP                    \ square edge outside 1..MX-MAX -> the host/packed buffers (MX-CAP) would be indexed out of bounds
1 constant MX-GUARD                        \ canary cells allocated past each buffer's used region (an overflow tripwire, seeded by MX-CANARY-SEED)

\ ---- typed host/packed buffer ownership (heap-allocated once, LAZILY) --------
\ Large host/packed buffers are HEAP-allocated (mmap), not `allot`ed: at MX-MAX=512 the seven
\ arrays total ~11 MB, kept off the dictionary whose cumulative `allot` silently corrupts the
\ engine's data space past ~32 MB. Each name is a word pushing its heap base, so every call
\ site is unchanged. Allocated once, and only when MX-BUF-INIT is explicitly called.
private
variable MX-HA-P  variable MX-HB-P  variable MX-HREF-P  variable MX-HC-P
variable MX-PA-P  variable MX-PB-P  variable MX-PC-P
variable MX-INIT-CALLS                      \ monotone count of MX-BUF-INIT entries (side-effect-reachability observable)
public
: MX-HA   ( -- ptr a )  MX-HA-P @ ;
: MX-HB   ( -- ptr a )  MX-HB-P @ ;
: MX-HREF ( -- ptr a )  MX-HREF-P @ ;
: MX-HC   ( -- ptr a )  MX-HC-P @ ;
: MX-PA   ( -- ptr a )  MX-PA-P @ ;
: MX-PB   ( -- ptr a )  MX-PB-P @ ;
: MX-PC   ( -- ptr a )  MX-PC-P @ ;
: MX-BUF-READY? ( -- bool )  MX-HA-P @ 0 <> ;   \ import-safety probe: false until MX-BUF-INIT
: MX-BUF-INIT-CALLS ( -- n )  MX-INIT-CALLS @ ;   \ reach-count witness: how many times MX-BUF-INIT was entered (unchanged => never reached => no allocation), robust to a sibling member already holding the shared arena
: MX-BUF-INIT ( -- )                       \ heap-allocate the seven large buffers once (idempotent)
   MX-INIT-CALLS @ 1+ MX-INIT-CALLS !      \ count the entry BEFORE the idempotent guard so reach-ness is observable even when the arena is already allocated
   MX-HA-P @ if exit then
   MX-CAP MX-GUARD + MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS MX-HA-P !     \ +MX-GUARD canary cell past index MX-CAP-1
   MX-CAP MX-GUARD + MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS MX-HB-P !
   MX-CAP MX-GUARD + MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS MX-HREF-P !
   MX-CAP MX-GUARD + MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS MX-HC-P !
   MX-CAP 4 * MX-GUARD cells + MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop MX-PA-P !   \ +MX-GUARD canary cell past byte MX-CAP*4-1
   MX-CAP 4 * MX-GUARD cells + MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop MX-PB-P !
   MX-CAP 4 * MX-GUARD cells + MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop MX-PC-P ! ;

\ ---- overflow tripwires: a seeded sentinel just past each buffer's used region --
\ MX-CANARY-SEED stamps the sentinel into the guard cell of all seven buffers; after a
\ max-edge (n=MX-MAX) pass over the buffer-indexing words, MX-CANARY-INTACT? must still
\ hold - a single perturbed sentinel is an overflow past the used [0,MX-CAP) region.
private
$5A5A5A5A5A5A5A5A constant MX-CANARY-VAL
public
: MX-CANARY-SEED ( -- )
   MX-CANARY-VAL MX-HA   MX-CAP cells + !   MX-CANARY-VAL MX-HB   MX-CAP cells + !
   MX-CANARY-VAL MX-HREF MX-CAP cells + !   MX-CANARY-VAL MX-HC   MX-CAP cells + !
   MX-CANARY-VAL MX-PA   MX-CAP 4 * + !     MX-CANARY-VAL MX-PB   MX-CAP 4 * + !
   MX-CANARY-VAL MX-PC   MX-CAP 4 * + ! ;
: MX-CANARY-INTACT? ( -- bool )
   MX-HA   MX-CAP cells + @ MX-CANARY-VAL =   MX-HB   MX-CAP cells + @ MX-CANARY-VAL = and
   MX-HREF MX-CAP cells + @ MX-CANARY-VAL = and   MX-HC MX-CAP cells + @ MX-CANARY-VAL = and
   MX-PA   MX-CAP 4 * + @ MX-CANARY-VAL = and   MX-PB   MX-CAP 4 * + @ MX-CANARY-VAL = and
   MX-PC   MX-CAP 4 * + @ MX-CANARY-VAL = and ;

\ ---- state: current square edge + compare evidence --------------------------
variable MX-N                              \ current square edge (M=N=K)
variable MX-BADI                           \ first mismatching index (-1 = none)
create MX-MAXERR 1 cells allot             \ f64 max abs error over the compare

\ ---- edge-capacity guard: every buffer-indexing word validates its own domain --
\ The seven host/packed buffers hold MX-CAP = MX-MAX^2 elements; a square edge n indexes
\ them at [0, n*n), so only 1 <= n <= MX-MAX stays in bounds. Each MX-* word that indexes a
\ buffer guards its own domain (MX-CHECK-EDGE on MX-N, MX-CHECK-E on an explicit extent) so a
\ future consumer inherits the safety: a bare n>MX-MAX (tileable or not) dies named as MX-E-CAP
\ here, never OOB-writing. With n <= MX-MAX the products n*n (<=262144) and n*n*n (<=1.3e8) and
\ the byte size n*n*4 (<=1.05e6) all sit far below the 63-bit ceiling, so the domain guard makes
\ every downstream multiply overflow-UNREACHABLE (no wrap possible once the edge check passes).
: MX-EDGE-OK? ( n -- bool )  dup 0 >  swap MX-MAX <=  and ;    \ 1..MX-MAX
: MX-CHECK-EDGE ( -- )  MX-N @ MX-EDGE-OK? 0= if MX-E-CAP throw then ;   \ guard MX-N before any host/device index
: MX-CHECK-E ( e -- e )  dup 0 <  over MX-CAP >  or if MX-E-CAP throw then ;   \ guard an explicit extent against the buffer capacity

\ ---- element-exact fill / host reference / compare (integer, bit-exact) ------
: MX-FILL ( -- )                           \ deterministic varied small integers (1..13 / 1..11)
   MX-CHECK-EDGE
   MX-N @ {: n:n :}
   n 0 ?do  i {: r:n :}
      n 0 ?do
         r 3 * i 7 * + 13 mod 1+ s>f  MX-HA r n * i + T-SET
         r 5 * i 2 * + 11 mod 1+ s>f  MX-HB r n * i + T-SET
      loop
   loop ;
private
: MX-DOT ( n n -- r ) {: m:n col:n :}      \ sum_k A[m][k]*B[k][col] (exact in f64)
   0.0  MX-N @ 0 ?do
      MX-HA m MX-N @ * i + T-GET
      MX-HB i MX-N @ * col + T-GET
      f* f+
   loop ;
public
: MX-REF ( -- )
   MX-CHECK-EDGE
   MX-N @ {: n:n :}
   n 0 ?do  i {: m:n :}
      n 0 ?do  m i MX-DOT  MX-HREF m n * i + T-SET  loop
   loop ;
: MX-COMPARE ( -- n )                      \ mismatch count over n*n; sets MX-BADI, MX-MAXERR (zero tolerance)
   MX-CHECK-EDGE
   -1 MX-BADI !  0.0 MX-MAXERR !  0
   MX-N @ MX-N @ * 0 ?do
      MX-HC i T-GET  MX-HREF i T-GET  f-  fabs {: err:r :}
      err 0.0 f> if
         1+  MX-BADI @ 0 < if i MX-BADI ! then
         err MX-MAXERR @ f> if err MX-MAXERR ! then
      then
   loop ;

\ ---- dtype-dispatched host -> packed A,B (F64 host -> tf32/fp16/bf16 device) --
: MX-PACK-AB ( e -- )  MX-CHECK-E {: e:n :}
   MMA-BF16? if      MX-HA e MX-PA BF16-PACK  MX-HB e MX-PB BF16-PACK   \ F64>BF16 RNE
   else MMA-F16? if  MX-HA e MX-PA F16-PACK   MX-HB e MX-PB F16-PACK    \ F64->fp16
   else              MX-HA e MX-PA F32-PACK   MX-HB e MX-PB F32-PACK    \ F64->tf32
   then then ;

\ ---- device buffers + host<->device movement --------------------------------
variable MX-DA  variable MX-DB  variable MX-DC
: MX-DEV-ALLOC ( -- )                      \ A/B sized by dtype elem bytes (MMA-ESZ), C f32; from MX-N
   MX-CHECK-EDGE
   MX-N @ dup * {: e:n :}
   e MMA-ESZ * MX-DA PTXBENCH:DEVICE-ALLOC
   e MMA-ESZ * MX-DB PTXBENCH:DEVICE-ALLOC
   e 4 * MX-DC PTXBENCH:DEVICE-ALLOC ;
: MX-DEV-FREE ( -- )
   MX-DA @ 0 <> if MX-DA @ PTXBENCH:DEVICE-FREE then
   MX-DB @ 0 <> if MX-DB @ PTXBENCH:DEVICE-FREE then
   MX-DC @ 0 <> if MX-DC @ PTXBENCH:DEVICE-FREE then
   0 MX-DA !  0 MX-DB !  0 MX-DC ! ;
: MX-OWN ( -- )                            \ transfer the three device buffers to the active CUDA-SCOPE frame (freed on unwind, replaces MX-DEV-FREE)
   MX-DA PTXBENCH:OWN-DEV  MX-DB PTXBENCH:OWN-DEV  MX-DC PTXBENCH:OWN-DEV ;
: MX-HTOD-AB ( e -- )  MX-CHECK-E {: e:n :}   \ upload the packed A,B to the device buffers
   MX-DA @ MX-PA e MMA-ESZ * PTXBENCH:HTOD
   MX-DB @ MX-PB e MMA-ESZ * PTXBENCH:HTOD ;
: MX-DTOH-C ( e -- )  MX-CHECK-E {: e:n :}    \ read C back to the host and unpack f32 -> host f64
   MX-PC MX-DC @ e 4 * PTXBENCH:DTOH
   MX-PC e MX-HC F32-UNPACK ;

\ ---- 2D grid/block + kernel params for the applied config at MX-N -----------
: MX-PARAMS ( -- )
   16 PTXBENCH:BLOCK!  MMA-NTHREADS 16 / PTXBENCH:BLOCKY!   \ 16x16=256 thr (8 warps) or 16x8=128 thr (4 warps)
   MX-N @ MMA-BN @ / PTXBENCH:GRID!  MX-N @ MMA-BROWS / PTXBENCH:GRIDY!   \ gridX = N/BN; gridY = M/BROWS
   36 PTXBENCH:PARAM-BYTES!
   MMA-DYNSMEM @ if MMA-SH-BYTES PTXBENCH:SHARED! else 0 PTXBENCH:SHARED! then   \ dynamic .shared (epilogue may grow SH)
   PTXBENCH:PREPARE-LAUNCH
   0 MX-DA PTXBENCH:PARAM-PTR!  8 MX-DB PTXBENCH:PARAM-PTR!  16 MX-DC PTXBENCH:PARAM-PTR!
   24 MX-N PTXBENCH:PARAM-U32!  28 MX-N PTXBENCH:PARAM-U32!  32 MX-N PTXBENCH:PARAM-U32! ;

\ ---- emit-time ptxas assemble of the applied config's captured PTX ----------
private
create MX-QO $1000 allot   create MX-QE $1000 allot
public
: MX-ASSEMBLE ( -- )
   ATGT:LABEL$ PTXTC:TC-ARCH!               \ assembler arch from the probed active target (sm_87 Orin / sm_121a GB10)
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   MX-QO $1000 >LEN MX-QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT {: rc:n :}
   rc 0= 0= if s" mma-exact: ptxas failed" 1 die then ;

\ ---- zero-block / ragged-M launch-shape guard (device-independent) ----------
\ At M < BROWS gridY = M/BROWS = 0 so the kernel NEVER runs and C reads back unchanged; at a
\ non-multiple M the tail rows are silently never computed. Both are meaningless measurements,
\ so require M to be a positive exact multiple of the M block rows (and N block cols) before any launch.
: MX-SHAPE-OK? ( n -- bool ) {: n:n :}
   n MMA-BROWS <  0=                        \ n >= BROWS (else gridY = 0, kernel never runs)
   n MMA-BROWS mod 0=  and                  \ exact multiple of BROWS (else the tail M rows are never computed)
   n MMA-BN @ mod 0=  and ;                 \ exact multiple of BN (else the tail N cols are never computed)
: MX-CHECK-SHAPE ( n -- )                   \ throw the named code on a zero-block / ragged-M launch shape
   MX-SHAPE-OK? 0= if MX-E-ZEROBLK throw then ;

;package
