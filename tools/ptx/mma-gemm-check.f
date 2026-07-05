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

128 constant MGC-MAX                       \ largest square edge (buffers sized for this)
MGC-MAX MGC-MAX * constant MGC-CAP         \ 16384 elems

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
create MGC-MAXERR 1 cells allot

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
   n 64 / PTXBENCH:GRID!  n 64 / PTXBENCH:GRIDY!
   36 PTXBENCH:PARAM-BYTES!
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

: MGC-ONE ( n -- ) {: n:n :}               \ one square GEMM correctness run
   n MGC-N !
   MGC-FILL  MGC-REF  MGC-LAUNCH  MGC-COMPARE {: bad:n :}
   s" MMM " type n . s" x" type n . s" x" type n . s"  : " type
   bad 0= if
      s" PASS element-exact (" type n n * . s"  cells, C[0][0]=" type
      MGC-HC 0 T-GET f>s . s" )" type cr
   else
      s" FAIL mismatches=" type bad . s"  first=" type MGC-BADI @ .
      s"  maxerr=" type MGC-MAXERR @ f>s . cr
   then ;

public
: MGC-ALL ( -- )
   CUDA:OPEN? 0= if s" mma-gemm-check: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   s" == TF32 mma.sync GEMM device-correctness (element-exact vs host) ==" type cr
   s" habu-mma-gemm-check" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   MGC-ASSEMBLE
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" MMM" PTXBENCH:KERNEL!  s" MMM" PTXBENCH:LABEL!
   PTXBENCH:OPEN  PTXBENCH:LOAD
   64 MGC-ONE
   128 MGC-ONE
   PTXBENCH:UNLOAD  PTXBENCH:CLOSE
   PTXTC:CLEAN ;

end-package

MMAGEMMCHECK:MGC-ALL
