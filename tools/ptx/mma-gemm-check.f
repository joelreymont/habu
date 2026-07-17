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
   16 PTXBENCH:BLOCK!  16 PTXBENCH:BLOCKY!
   n 64 / PTXBENCH:GRID!  n MMA-BROWS / PTXBENCH:GRIDY!   \ gridY = M/block-rows (BROWS=64*MFRAGS)
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

public
: MGC-ALL ( -- )
   MGC-SMEM-NEG                                        \ emitter fail-closed check (device-independent)
   MGC-ZEROBLK-NEG                                     \ zero-block/ragged-M launch guard (device-independent)
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
   0 MMA-LMODE ! ;                                     \ restore the committed default (baseline scalar+cvt)

;package

MMAGEMMCHECK:MGC-ALL
