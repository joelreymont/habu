\ mma-gemm-check.f - device-correctness of the TF32 mma.sync GEMM (lib/ptx/cg-mma.f MMM).
\
\ dot habu-tensor-core-mma, task B/C. tools/ptx/mma-probe.f proved ONE mma tile element-
\ exact; this proves the FULL K-looping warp-tiled kernel EMIT-MATMUL-MMA (cp.async
\ staging + accumulation across K + the warp/D-fragment STORE mapping) element-exact vs a
\ host matmul reference at 64^3 (1 block, 2 K-tiles) and 128^3 (2x2 blocks, 4 K-tiles).
\
\ Element-EXACT design (as in mma-probe): small integer A,B entries so every value is exact
\ in tf32's 10-bit mantissa and every 64/128-long f32 accumulation stays < 2^24 - a correct
\ kernel reproduces the integer matmul BIT-EXACT. Varied (non-constant) entries so a store
\ mis-mapping (transposed tile, swapped warp row/col) mismatches instead of hiding behind a
\ symmetric all-ones tile. This is the committed correctness proof for the BENCH kernel.
\
\ Device-only: off the Orin (no libcuda) MGC-ALL SKIPS so this file still check-loads. Run
\ on the device (ISOLATED-COPY): bin/hb --load tools/ptx/mma-gemm-check.f

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
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

package MMAGEMMCHECK

512 constant MGC-MAX                       \ largest square edge (buffers sized for this; 512 for the wide 256-row MFRAGS=4 block)
MGC-MAX MGC-MAX * constant MGC-CAP         \ 262144 elems

create MGC-HA MGC-CAP cells allot          \ host A (f64)
create MGC-HB MGC-CAP cells allot          \ host B
create MGC-HREF MGC-CAP cells allot        \ host reference (f64)
create MGC-HC MGC-CAP cells allot          \ device C read back (f64)
create MGC-PA MGC-CAP 4 * allot            \ packed f32
create MGC-PB MGC-CAP 4 * allot
create MGC-PC MGC-CAP 4 * allot
create MGC-QO $1000 allot  create MGC-QE $1000 allot
variable MGC-DA  variable MGC-DB  variable MGC-DC
variable MGC-N   variable MGC-BADI
variable MGC-SA  variable MGC-SB            \ the two square edges each config is checked at (default 64/128)
create MGC-MAXERR 1 cells allot
64 MGC-SA !  128 MGC-SB !
-6101 constant MGC-E-ZEROBLK               \ launch M < the block rows (BROWS) or a ragged multiple -> silent zero/partial C

\ deterministic varied small integers (1..13 / 1..11), distinct enough to catch mis-mapping
: MGC-FILL ( -- ) {: :}
   MGC-N @ {: n:n :}
   n 0 ?do  i {: r:n :}
      n 0 ?do
         r 3 * i 7 * + 13 mod 1+ s>f  MGC-HA r n * i + T-SET
         r 5 * i 2 * + 11 mod 1+ s>f  MGC-HB r n * i + T-SET
      loop
   loop ;

: MGC-DOT ( n n -- r ) {: m:n col:n :}     \ sum_k A[m][k]*B[k][col] (exact in f64)
   0.0  MGC-N @ 0 ?do
      MGC-HA m MGC-N @ * i + T-GET
      MGC-HB i MGC-N @ * col + T-GET
      f* f+
   loop ;
: MGC-REF ( -- )
   MGC-N @ {: n:n :}
   n 0 ?do  i {: m:n :}
      n 0 ?do  m i MGC-DOT  MGC-HREF m n * i + T-SET  loop
   loop ;

: MGC-ASSEMBLE ( -- )
   ATGT:LABEL$ PTXTC:TC-ARCH!          \ assembler arch from the probed active target (sm_87 Orin / sm_121a GB10)
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   MGC-QO $1000 >LEN MGC-QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT {: rc:n :}
   rc 0= 0= if s" mma-gemm-check: ptxas failed" 1 die then ;

: MGC-LAUNCH ( -- )                        \ alloc + htod + launch (block 16x16, grid (n/64)^2) + dtoh
   MGC-N @ {: n:n :}  n n * {: e:n :}
   e 4 * MGC-DA PTXBENCH:DEVICE-ALLOC
   e 4 * MGC-DB PTXBENCH:DEVICE-ALLOC
   e 4 * MGC-DC PTXBENCH:DEVICE-ALLOC
   MGC-HA e MGC-PA F32-PACK
   MGC-HB e MGC-PB F32-PACK
   MGC-DA @ MGC-PA e 4 * PTXBENCH:HTOD
   MGC-DB @ MGC-PB e 4 * PTXBENCH:HTOD
   16 PTXBENCH:BLOCK!  MMA-NTHREADS 16 / PTXBENCH:BLOCKY!   \ 16x16=256 thr (8 warps) or 16x8=128 thr (4 warps)
   n 64 / PTXBENCH:GRID!  n MMA-BROWS / PTXBENCH:GRIDY!   \ gridY = M/block-rows (BROWS=WROWS*16*MFRAGS)
   36 PTXBENCH:PARAM-BYTES!
   MMA-DYNSMEM @ if MMA-SMEM PTXBENCH:SHARED! else 0 PTXBENCH:SHARED! then   \ dynamic .shared tile
   PTXBENCH:PREPARE-LAUNCH
   0  MGC-DA PTXBENCH:PARAM-PTR!
   8  MGC-DB PTXBENCH:PARAM-PTR!
   16 MGC-DC PTXBENCH:PARAM-PTR!
   24 MGC-N PTXBENCH:PARAM-U32!  28 MGC-N PTXBENCH:PARAM-U32!  32 MGC-N PTXBENCH:PARAM-U32!
   PTXBENCH:LAUNCH  PTXBENCH:SYNC
   MGC-PC MGC-DC @ e 4 * PTXBENCH:DTOH
   MGC-PC e MGC-HC F32-UNPACK
   MGC-DA @ PTXBENCH:DEVICE-FREE  MGC-DB @ PTXBENCH:DEVICE-FREE  MGC-DC @ PTXBENCH:DEVICE-FREE ;

: MGC-COMPARE ( -- n )                     \ mismatch count over n*n; sets MGC-BADI, MGC-MAXERR
   -1 MGC-BADI !  0.0 MGC-MAXERR !  0
   MGC-N @ MGC-N @ * 0 ?do
      MGC-HC i T-GET  MGC-HREF i T-GET  f-  fabs {: err:r :}
      err 0.0 f> if
         1+  MGC-BADI @ 0 < if i MGC-BADI ! then
         err MGC-MAXERR @ f> if err MGC-MAXERR ! then
      then
   loop ;

\ HARDENING (dot habu-mma-wave-2): fail closed on a zero-block / ragged-M launch. The launch grid is
\ gridY = M/BROWS (BROWS = 64*MFRAGS, MMA-BROWS). At M < BROWS gridY=0 so the kernel NEVER runs and C
\ is read back unchanged (all-zero at a zeroed C), which a naive check could mistake for a correct
\ small GEMM; at a non-multiple M the tail rows are silently never computed. Both are meaningless
\ measurements, so require M to be a positive exact multiple of the M block rows before any launch.
: MGC-SHAPE-OK? ( n -- bool ) {: n:n :}    \ n is a positive exact multiple of the M block rows
   n MMA-BROWS <  0=                        \ n >= BROWS  (else gridY = n/BROWS = 0, kernel never runs)
   n MMA-BROWS mod 0=  and ;                \ exact multiple (else the tail M rows are never computed)
: MGC-CHECK-SHAPE ( n -- )                  \ throw the named code on a zero-block / ragged-M launch shape
   MGC-SHAPE-OK? 0= if MGC-E-ZEROBLK throw then ;

: MGC-ONE ( n -- ) {: n:n :}               \ one square GEMM correctness run
   n MGC-N !
   n MGC-CHECK-SHAPE                        \ refuse a zero-block / ragged-M shape (else the row is meaningless)
   MGC-FILL  MGC-REF  MGC-LAUNCH  MGC-COMPARE {: bad:n :}
   s" MMM " type n . s" x" type n . s" x" type n . s"  : " type
   bad 0= if
      s" PASS element-exact (" type n n * . s"  cells, C[0][0]=" type
      MGC-HC 0 T-GET f>s . s" )" type cr
   else
      s" FAIL mismatches=" type bad . s"  first=" type MGC-BADI @ .
      s"  maxerr=" type MGC-MAXERR @ f>s . cr
   then ;

\ negative regression (dot habu-mma-larger-bk): an over-budget STATIC .shared tile must fail closed
\ in the emitter with the named E-MMA-SMEM, not silently emit an illegal kernel. BK=64 stages=2
\ static = 65536 B > the 48 KiB sm_87 cap; the dynamic (.extern .shared) path is exempt.
: MGC-TRY-EMIT ( -- n )  [: PTX-CAPTURE-ON EMIT-MATMUL-MMA PTX-CAPTURE-OFF ;] catch ;
: MGC-SMEM-NEG ( -- )
   64 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !
   MGC-TRY-EMIT {: rc:n :}
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !
   s" -- emitter SMEM legality: BK=64 stages=2 static -> " type
   rc E-MMA-SMEM = if s" fail-closed E-MMA-SMEM (PASS)" type else s" NOT fail-closed rc=" type rc . s"  (FAIL)" type then cr ;

\ negative+positive regression (dot habu-mma-wave-3): a B-ldmatrix config with a BT row stride that is
\ not a 16 B multiple (BK+BPAD not a multiple of 4) must fail closed in the EMITTER with E-MMA-BLDM, not
\ emit a kernel whose ldmatrix rows are misaligned and fault the GPU at launch. bpad=2 -> BTROW=34*4=136 B
\ (not 16-aligned) must throw; bpad=4 -> 36*4=144 B (aligned) must emit; MFRAGS=1+BLDM must throw. Device-
\ independent (pure emit). This is the guard that keeps a bad knob from ever reaching a faulting launch.
: MGC-BLDM-NEG ( -- )
   4 MMA-MFRAGS !  32 MMA-BK !  8 MMA-PAD !  1 MMA-STAGES !  1 MMA-DYNSMEM !  2 MMA-LMODE !  1 MMA-BLDM !
   2 MMA-BPAD !  MGC-TRY-EMIT {: r2:n :}                  \ bpad=2 -> misaligned BT row -> must throw E-MMA-BLDM
   4 MMA-BPAD !  MGC-TRY-EMIT {: r4:n :}                  \ bpad=4 -> 16 B aligned -> must emit (0)
   1 MMA-MFRAGS !  4 MMA-BPAD !  MGC-TRY-EMIT {: rm1:n :}  \ MFRAGS=1 + BLDM -> wide-path-only -> must throw E-MMA-BLDM
   1 MMA-MFRAGS !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !  0 MMA-BLDM !  0 MMA-BPAD !
   s" -- B-ldmatrix legality: bpad=2->" type r2 . s"  bpad=4->" type r4 . s"  MFRAGS=1->" type rm1 . cr
   r2 E-MMA-BLDM =  r4 0=  and  rm1 E-MMA-BLDM =  and
   if s" -- B-ldmatrix legality: fail-closed on misaligned BT row + MFRAGS=1, emits when aligned (PASS)" type cr
   else s" mma-gemm-check: B-ldmatrix legality regression FAILED" 1 die then ;

\ negative+positive regression (dot habu-mma-wave-2): the zero-block / ragged-M launch guard must throw
\ MGC-E-ZEROBLK below BROWS and at a non-multiple, and pass on exact multiples. Device-independent.
variable MGC-TN
: MGC-TRY-SHAPE ( n -- n )  MGC-TN !  [: MGC-TN @ MGC-CHECK-SHAPE ;] catch ;   \ 0 = ok, else throw code
: MGC-ZEROBLK-NEG ( -- )
   2 MMA-MFRAGS !                                   \ BROWS = 128
   64  MGC-TRY-SHAPE {: r64:n :}                     \ 64 < 128 -> gridY=0 -> must throw
   128 MGC-TRY-SHAPE {: r128:n :}                    \ exact 1 M-block -> ok
   192 MGC-TRY-SHAPE {: r192:n :}                    \ 192 % 128 = 64 -> ragged tail -> must throw
   256 MGC-TRY-SHAPE {: r256:n :}                    \ exact 2 M-blocks -> ok
   1 MMA-MFRAGS !
   s" -- zero-block guard (MFRAGS=2, BROWS=128): 64->" type r64 . s"  128->" type r128 .
   s"  192->" type r192 . s"  256->" type r256 . cr
   r64 MGC-E-ZEROBLK =  r192 MGC-E-ZEROBLK =  and  r128 0=  and  r256 0=  and
   if s" -- zero-block guard: fail-closed on <BROWS and ragged, pass on exact multiples (PASS)" type cr
   else s" mma-gemm-check: zero-block guard regression FAILED" 1 die then ;

\ one fragment-load mode: set MMA-LMODE, re-emit + re-assemble + re-load MMM, check 64^3 + 128^3.
: MGC-MODE ( n -- ) {: mode:n :}
   mode MMA-LMODE !
   s" -- MMM fragment-load mode " type mode .
   mode 0= if s" (scalar+cvt baseline)" type then
   mode 1 = if s" (scalar raw, no cvt)" type then
   mode 2 = if s" (ldmatrix.x4 A + raw B, no cvt)" type then
   cr
   s" habu-mma-gemm-check" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   MGC-ASSEMBLE
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" MMM" PTXBENCH:KERNEL!  s" MMM" PTXBENCH:LABEL!
   PTXBENCH:OPEN  PTXBENCH:LOAD
   MGC-SA @ MGC-ONE
   MGC-SB @ MGC-ONE
   PTXBENCH:UNLOAD  PTXBENCH:CLOSE
   PTXTC:CLEAN ;

\ one larger-BK / swizzled tile config (dot habu-mma-larger-bk): set the tile knobs, run one
\ fragment-load mode through MGC-MODE (re-emit + assemble + check 64^3 + 128^3), restore defaults.
: MGC-CFG ( n n n n n -- ) {: bk:n pad:n stages:n dyn:n mode:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !
   s" -- config BK=" type bk .  s"  pad=" type pad .  s"  stages=" type stages .
   s"  dyn=" type dyn . s" :" type cr
   mode MGC-MODE
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM ! ;   \ restore the committed defaults

\ WIDER-M config (dot habu-mma-amortize-the, MFRAGS=4 dot habu-mma-wave-2): MFRAGS>1 grows the block to
\ 64*MFRAGS rows, so it is checked at the two block-M-aware edges BROWS (1 M-block, all N-blocks) and
\ 2*BROWS (multiple M-blocks) - 128^3/256^3 at MFRAGS=2, 256^3/512^3 at MFRAGS=4. A shape below BROWS
\ would launch zero M-blocks (now the MGC-CHECK-SHAPE guard throws). Restores MFRAGS=1 and the 64/128 edges.
: MGC-CFG-WIDE ( n n n n n n -- ) {: bk:n pad:n stages:n dyn:n mode:n mfrags:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mfrags MMA-MFRAGS !
   MMA-BROWS MGC-SA !  MMA-BROWS 2 * MGC-SB !          \ block-M-aware edges from BROWS (128/256 at MFRAGS=2, 256/512 at MFRAGS=4)
   s" -- WIDE config MFRAGS=" type mfrags .  s"  BK=" type bk .  s"  pad=" type pad .
   s"  stages=" type stages .  s"  dyn=" type dyn . s"  (" type MGC-SA @ . s" ^3," type MGC-SB @ . s" ^3):" type cr
   mode MGC-MODE
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  64 MGC-SA !  128 MGC-SB ! ;

\ B-SIDE ldmatrix config (dot habu-mma-wave-3): transposed-Bs staging + ldmatrix.x2 B fragments. Checked
\ mode 2 (A + B ldmatrix) at the block-M-aware edges; the same shapes are also checked scalar+cvt (mode 0
\ MGC-CFG-WIDE legs) as the RNE-exact cross-reference, so agreeing element-exact vs the host ref proves both
\ lmodes agree. Restores MFRAGS=1 / BLDM=0 / BPAD=0 and the 64/128 edges.
: MGC-CFG-WIDE-B ( n n n n n n -- ) {: bk:n pad:n stages:n dyn:n mfrags:n bpad:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mfrags MMA-MFRAGS !
   1 MMA-BLDM !  bpad MMA-BPAD !
   MMA-BROWS MGC-SA !  MMA-BROWS 2 * MGC-SB !
   s" -- WIDE-B (B-ldmatrix transposed Bs) MFRAGS=" type mfrags .  s"  BK=" type bk .  s"  pad=" type pad .
   s"  bpad=" type bpad .  s"  stages=" type stages .  s"  dyn=" type dyn .
   s"  (" type MGC-SA @ . s" ^3," type MGC-SB @ . s" ^3):" type cr
   2 MGC-MODE
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !
   0 MMA-BLDM !  0 MMA-BPAD !  64 MGC-SA !  128 MGC-SB ! ;

\ 4-WARP (2x2 warp grid, 128 threads) configs (dot habu-4-warp-mma). The narrower grid halves the
\ per-block threads and smem while keeping the SAME per-warp fragment/accumulator/store maps, so the
\ only new thing to prove element-exact is the WARPS=4 thread-count staging partition (MMA-NTHREADS=128).
\ These wrappers set MMA-WARPS=4, delegate to the shared wide-config machinery (which computes the
\ block-M-aware check edges from BROWS = WROWS*16*MFRAGS, e.g. 128^3/256^3 at MFRAGS=4), then restore
\ the 8-warp default. Mirrors Triton's per-shape tf32 winner blocking (BM128xBN64, docs/eval-triton.md GB10).
: MGC-CFG-W4 ( n n n n n n -- )                        \ bk pad stages dyn mode mfrags, 4-warp scalar/A-ldmatrix
   4 MMA-WARPS !  s" -- 4-WARP (2x2 grid, 128 thr):" type cr
   MGC-CFG-WIDE  8 MMA-WARPS ! ;
: MGC-CFG-W4-B ( n n n n n n -- )                      \ bk pad stages dyn mfrags bpad, 4-warp B-ldmatrix
   4 MMA-WARPS !  s" -- 4-WARP (2x2 grid, 128 thr) B-ldmatrix:" type cr
   MGC-CFG-WIDE-B  8 MMA-WARPS ! ;

\ DEEP-STAGE 4-warp configs (dot habu-4-warp-mma step 3). N>=3 uses the N-stage ring pipeline
\ (lib/ptx/cg-mma.f MMA-PIPE-KLOOP-MULTI), whose steady wait_group(N-1) and draining epilogue
\ wait_group(N-2..0) are only exact when T=ceil(K/BK) >= N-1. The BROWS-derived edges (down to 64
\ at MFRAGS=2) can give T<N-1 for N>3, so these check at fixed 256^3/512^3 (T=8/16 at BK=32, >= N-1
\ for every N swept) - both exact multiples of the 64/128 M-blocks. The narrower 4-warp footprint is
\ what lets 3-5 full smem buffers fit under the 99 KB cap. Restores the 8-warp default + 64/128 edges.
: MGC-CFG-W4S ( n n n n n n -- ) {: bk:n pad:n stages:n dyn:n mode:n mfrags:n :}   \ deep-stage A-ldmatrix/scalar
   4 MMA-WARPS !
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mfrags MMA-MFRAGS !
   256 MGC-SA !  512 MGC-SB !
   s" -- 4-WARP deep-stage stages=" type stages .  s"  MFRAGS=" type mfrags .  s"  (256^3,512^3):" type cr
   mode MGC-MODE
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  64 MGC-SA !  128 MGC-SB !  8 MMA-WARPS ! ;
: MGC-CFG-W4S-B ( n n n n n n -- ) {: bk:n pad:n stages:n dyn:n mfrags:n bpad:n :}   \ deep-stage B-ldmatrix
   4 MMA-WARPS !
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mfrags MMA-MFRAGS !
   1 MMA-BLDM !  bpad MMA-BPAD !
   256 MGC-SA !  512 MGC-SB !
   s" -- 4-WARP deep-stage B-ldmatrix stages=" type stages .  s"  MFRAGS=" type mfrags .  s"  bpad=" type bpad .  s"  (256^3,512^3):" type cr
   2 MGC-MODE
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  0 MMA-BLDM !  0 MMA-BPAD !  64 MGC-SA !  128 MGC-SB !  8 MMA-WARPS ! ;

\ negative regression (dot habu-4-warp-mma): the warp-grid guard must fail closed with E-MMA-WARPS on an
\ unsupported warp count and on WARPS=4 without the wide (MFRAGS>1) staging, and emit cleanly on the two
\ legal grids. Device-independent (pure emit). Keeps a bad warp knob from ever reaching a launch.
: MGC-WARPS-NEG ( -- )
   6 MMA-WARPS !  32 MMA-BK !  8 MMA-PAD !  2 MMA-STAGES !  1 MMA-DYNSMEM !  2 MMA-LMODE !  4 MMA-MFRAGS !
   MGC-TRY-EMIT {: r6:n :}                               \ WARPS=6 -> unsupported grid -> must throw E-MMA-WARPS
   4 MMA-WARPS !  1 MMA-MFRAGS !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !
   MGC-TRY-EMIT {: r4m1:n :}                             \ WARPS=4 + MFRAGS=1 -> non-wide staging -> must throw
   4 MMA-WARPS !  4 MMA-MFRAGS !  8 MMA-PAD !  1 MMA-STAGES !  1 MMA-DYNSMEM !  2 MMA-LMODE !
   MGC-TRY-EMIT {: r4m4:n :}                             \ WARPS=4 + MFRAGS=4 wide -> must emit (0)
   8 MMA-WARPS !  1 MMA-MFRAGS !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !
   s" -- warp-grid legality: WARPS=6->" type r6 . s"  WARPS=4+MFRAGS=1->" type r4m1 . s"  WARPS=4+MFRAGS=4->" type r4m4 . cr
   r6 E-MMA-WARPS =  r4m1 E-MMA-WARPS =  and  r4m4 0=  and
   if s" -- warp-grid legality: fail-closed on bad warp count + non-wide 4-warp, emits on legal grids (PASS)" type cr
   else s" mma-gemm-check: warp-grid legality regression FAILED" 1 die then ;

public
: MGC-ALL ( -- )
   MGC-SMEM-NEG                                        \ emitter fail-closed check (device-independent)
   MGC-ZEROBLK-NEG                                     \ zero-block/ragged-M launch guard (device-independent)
   MGC-BLDM-NEG                                        \ B-ldmatrix misaligned/MFRAGS=1 fail-closed (device-independent)
   MGC-WARPS-NEG                                       \ warp-grid legality fail-closed (device-independent)
   CUDA:OPEN? 0= if s" mma-gemm-check: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   s" == TF32 mma.sync GEMM device-correctness (element-exact vs host) ==" type cr
   0 MGC-MODE  1 MGC-MODE  2 MGC-MODE
   s" == larger-BK / swizzled shared configs (dot habu-mma-larger-bk) ==" type cr
   64 0 1 0 0 MGC-CFG                                  \ BK=64 single-buffer static, scalar+cvt
   64 0 1 0 2 MGC-CFG                                  \ BK=64 single-buffer static, ldmatrix
   64 0 2 1 0 MGC-CFG                                  \ BK=64 double-buffer DYNAMIC smem, scalar+cvt
   64 0 2 1 2 MGC-CFG                                  \ BK=64 double-buffer DYNAMIC smem, ldmatrix
   32 8 2 0 0 MGC-CFG                                  \ BK=32 padded (bank-swizzled As), scalar+cvt
   32 8 2 0 2 MGC-CFG                                  \ BK=32 padded, ldmatrix (bank-free fragment rows)
   64 8 2 1 2 MGC-CFG                                  \ BK=64 padded double-buffer DYNAMIC, ldmatrix
   s" == wider-M register tile configs (dot habu-mma-amortize-the) ==" type cr
   32 8 2 1 2 2 MGC-CFG-WIDE                           \ MFRAGS=2 BK=32 pad=8 double-buffer DYNAMIC ldmatrix (128x64)
   32 8 1 0 2 2 MGC-CFG-WIDE                           \ MFRAGS=2 BK=32 pad=8 SINGLE-buffer STATIC ldmatrix (128x64)
   32 8 2 1 0 2 MGC-CFG-WIDE                           \ MFRAGS=2 BK=32 pad=8 double-buffer DYNAMIC scalar+cvt (exact-RNE cross-check)
   s" == MFRAGS=4 register tile configs (dot habu-mma-wave-2, 256x64 block) ==" type cr
   32 8 2 1 2 4 MGC-CFG-WIDE                           \ MFRAGS=4 BK=32 pad=8 double-buffer DYNAMIC ldmatrix (256x64, 98304 B)
   32 8 2 1 0 4 MGC-CFG-WIDE                           \ MFRAGS=4 BK=32 pad=8 double-buffer DYNAMIC scalar+cvt (exact-RNE cross-check)
   s" == B-side ldmatrix transposed-Bs configs (dot habu-mma-wave-3) ==" type cr
   32 8 1 1 4 0 MGC-CFG-WIDE-B                         \ MFRAGS=4 bpad=0 single-buffer DYN B-ldmatrix (256x64; bank-aliased read, fits static budget)
   32 8 1 1 4 4 MGC-CFG-WIDE-B                         \ MFRAGS=4 bpad=4 single-buffer DYN B-ldmatrix (256x64; conflict-free read stride 36)
   32 8 2 1 4 4 MGC-CFG-WIDE-B                         \ MFRAGS=4 bpad=4 double-buffer DYN B-ldmatrix (256x64)
   32 8 2 1 2 4 MGC-CFG-WIDE-B                         \ MFRAGS=2 bpad=4 double-buffer DYN B-ldmatrix (128x64)
   32 8 1 1 2 4 MGC-CFG-WIDE-B                         \ MFRAGS=2 bpad=4 SINGLE-buffer DYN B-ldmatrix (128x64; GB10 1024^3 winner)
   32 8 1 0 2 4 MGC-CFG-WIDE-B                         \ MFRAGS=2 bpad=4 SINGLE-buffer STATIC B-ldmatrix (128x64, 29696 B; GB10 1024^3 winner static)
   s" == 4-warp (2x2 grid, 128-thread) tile configs (dot habu-4-warp-mma) ==" type cr
   32 8 1 0 2 4 MGC-CFG-W4                             \ 4-warp MFRAGS=4 BK=32 pad=8 stages=1 STATIC ldmatrix-A (BM128xBN64, 28672 B)
   32 8 2 1 2 4 MGC-CFG-W4                             \ 4-warp MFRAGS=4 BK=32 pad=8 stages=2 DYN ldmatrix-A (BM128xBN64, 57344 B)
   32 8 2 1 0 4 MGC-CFG-W4                             \ 4-warp MFRAGS=4 scalar+cvt (exact-RNE cross-check of the same tile)
   32 8 1 1 4 4 MGC-CFG-W4-B                           \ 4-warp MFRAGS=4 bpad=4 stages=1 DYN B-ldmatrix (BM128xBN64, 29696 B)
   32 8 2 1 4 4 MGC-CFG-W4-B                           \ 4-warp MFRAGS=4 bpad=4 stages=2 DYN B-ldmatrix (BM128xBN64, 59392 B)
   32 8 1 0 2 2 MGC-CFG-W4                             \ 4-warp MFRAGS=2 BK=32 pad=8 stages=1 STATIC ldmatrix-A (BM64xBN64, 18432 B)
   s" == 4-warp DEEP-STAGE (N-stage ring pipeline) configs (dot habu-4-warp-mma step 3) ==" type cr
   32 8 3 1 2 4 MGC-CFG-W4S                            \ 4-warp MFRAGS=4 stages=3 DYN ldmatrix-A (BM128xBN64, 86016 B)
   32 8 3 1 2 2 MGC-CFG-W4S                            \ 4-warp MFRAGS=2 stages=3 DYN ldmatrix-A (BM64xBN64, 55296 B)
   32 8 3 1 0 2 MGC-CFG-W4S                            \ 4-warp MFRAGS=2 stages=3 scalar+cvt (exact-RNE cross-check of the ring pipeline)
   32 8 4 1 2 2 MGC-CFG-W4S                            \ 4-warp MFRAGS=2 stages=4 DYN ldmatrix-A (BM64xBN64, 73728 B)
   32 8 5 1 2 2 MGC-CFG-W4S                            \ 4-warp MFRAGS=2 stages=5 DYN ldmatrix-A (BM64xBN64, 92160 B)
   32 8 3 1 4 4 MGC-CFG-W4S-B                          \ 4-warp MFRAGS=4 bpad=4 stages=3 DYN B-ldmatrix (BM128xBN64, 89088 B)
   0 MMA-LMODE ! ;                                     \ restore the committed default (baseline scalar+cvt)

;package

MMAGEMMCHECK:MGC-ALL
