\ device-gold.f - COMMITTED device-correctness goldens for the flagship kernels.
\
\ GAP #4 (dot habu-committed-device-correctness): the flagship kernels - the tiled
\ SGEMM (lib/ptx/cg-matmul.f MM), the fused attention (lib/ptx/cg-attention.f ATTN),
\ the v4 fused chain relu(a*x+y) (tools/ptx/fused-relu-cg.f SAXPY), and the memory-
\ bandwidth SAXPY (tools/ptx/saxpy-cg.f, the kernel bandwidth.f benchmarks) - were
\ device-VERIFIED only by throwaway /tmp runners + python, so a regression in the
\ emitted PTX was caught by NOTHING in the committed tree. This harness closes that:
\ for EACH flagship kernel it spawns bin/hb to emit the COMMITTED self-emitting entry
\ file (tools/ptx/{matmul,attention,fused-relu,saxpy}-cg.f), ptxas-assembles it, loads
\ + launches it on the Orin, and compares the device output against a COMMITTED CPU
\ golden on deterministic exact-binary-fraction inputs. A wrong kernel FAILS its golden
\ (demonstrated by the corruption probe in the dot: dropping RELU-V4 from the fused
\ producer makes the fused golden's clamped lanes diverge).
\
\ Complements maki/eval-emit-device.f: that grades the EVAL autograder's CANDIDATE
\ bodies (K driver reconstructions) for the sumnorm/gemm/attention tasks; THIS proves
\ the exact committed producer entry files (EMIT-MATMUL / ATTN:EMIT / EMIT-FUSED /
\ EMIT-SAXPY) - a regression editing those committed emit paths is caught here.
\
\ Off-device (no libcuda) it is a recorded SKIP and still check-loads; the emit halves
\ are host-proven in tools/ptx/device-gold-test.f (the gpu-emit-test pattern), so the
\ ptx-toolchain suite compile-checks this file. It requires the CUDA driver FFI, so it
\ is a SPAWN-ONLY ptx-toolchain member (it SIGBUSes in the resident runner image, like
\ the other device-leg tools) and DEVICE-runs on zed via one command:
\   cd ~/Work/habu && ./bin/hb --load tools/ptx/device-gold.f
\ Fully checked Habu; no 0 set-check. Load after the PTX codegen + CUDA driver stack.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require lib/ffi.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f

package DEVGOLD

private

\ ---- capture + timeouts -----------------------------------------------------
$10000 constant CAP-OUT-CAP                    \ emitted PTX (single MM ~28 KB)
$2000  constant CAP-ERR-CAP
30000  constant EMIT-TIMEOUT-MS

create CAP-OUT CAP-OUT-CAP allot  create CAP-ERR CAP-ERR-CAP allot
create ASM-OUT CAP-ERR-CAP allot  create ASM-ERR CAP-ERR-CAP allot
variable EMIT-BYTES

\ ---- flagship kernel shapes (exact-binary-fraction inputs) ------------------
64 constant GEMM-M   64 constant GEMM-N   32 constant GEMM-K
GEMM-M GEMM-K * constant GM-A-ELEMS   GEMM-K GEMM-N * constant GM-B-ELEMS
GEMM-M GEMM-N * constant GM-C-ELEMS
GM-A-ELEMS 4 * constant GM-A-BYTES    GM-B-ELEMS 4 * constant GM-B-BYTES
GM-C-ELEMS 4 * constant GM-C-BYTES
GEMM-N 64 / constant GM-GRID-N        GEMM-M 64 / constant GM-GRID-M
36 constant GM-PARAM-BYTES                     \ pA,pB,pC(8) pM,pN,pK(4)

4 constant ATTN-N    2 constant ATTN-D
ATTN-N ATTN-D * constant AT-ELEMS     AT-ELEMS 4 * constant AT-BYTES
40 constant AT-PARAM-BYTES                     \ pQ,pK,pV,pO(8) pN,pD(4)

1024 constant SP-N                             \ SAXPY-family element count (v4: one 256-block, no tail)
SP-N 4 * constant SP-BYTES
256 constant SP-BLOCK
SP-N SP-BLOCK 4 * 1- + SP-BLOCK 4 * / constant FU-GRID    \ fused v4: ceil(n/1024) = 1
SP-N SP-BLOCK 1- + SP-BLOCK / constant BW-GRID            \ scalar bandwidth: ceil(n/256) = 4
24 constant SP-PARAM-BYTES                     \ pX,pY(8) a,n(4)
$40400000 constant FU-A-BITS                   \ fused scale a = 3.0
$40000000 constant BW-A-BITS                   \ bandwidth scale a = 2.0

\ ---- tolerances -------------------------------------------------------------
\ SGEMM: the K=32 accumulation is judged with the lower-golden (maki/lower-golden.f)
\ linear tolerance |dev - host_f32| <= atol + rtol*|host_f32| (the matmul class row).
\ attention/fused/bandwidth outputs are exact binary fractions, judged with the
\ eval-device scalar TOL (1/1024) that splits a correct kernel (err 0) from any
\ divergent one (err >= 2.0) wide.
0.000244140625 constant GM-ATOL                \ 1/4096
0.0009765625   constant GM-RTOL                \ 1/1024
0.0009765625   constant SCALAR-TOL             \ 1/1024 (attn / fused / bandwidth)

\ ---- host staging buffers ---------------------------------------------------
create GEMM-A GM-A-BYTES allot  create GEMM-B GM-B-BYTES allot  create GEMM-C GM-C-BYTES allot
create ATTN-Q AT-BYTES allot  create ATTN-K AT-BYTES allot
create ATTN-V AT-BYTES allot  create ATTN-O AT-BYTES allot
create SP-X SP-BYTES allot    create SP-Y SP-BYTES allot

\ ---- little-endian f32 host pack/unpack + float helpers ---------------------
: F32! ( n ptr a n -- ) {: v:n buf:ptr idx:n :}
   idx 4 * {: o:n :}
   v           $FF and  buf o     + c!
   v 8 rshift  $FF and  buf o 1 + + c!
   v 16 rshift $FF and  buf o 2 + + c!
   v 24 rshift $FF and  buf o 3 + + c! ;
: F32@ ( ptr a n -- n ) {: buf:ptr idx:n :}
   idx 4 * {: o:n :}
   buf o     + c@
   buf o 1 + + c@  8  lshift or
   buf o 2 + + c@  16 lshift or
   buf o 3 + + c@  24 lshift or ;
: F! ( r ptr a n -- ) {: v:r buf:ptr idx:n :}    \ store a Habu float as f32 at element idx
   v F64>F32 buf idx F32! ;
: MAXF ( r r -- r ) {: a:r b:r :}  a b f> if a else b then ;
: NARROW ( r -- r )  F64>F32 F32>F64 ;           \ round a host f64 onto the f32 grid
: WITHIN? ( r r r r -- bool ) {: dev:r host:r atol:r rtol:r :}
   atol  rtol host fabs f*  f+ {: tol:r :}       \ tol = atol + rtol*|host|
   dev host f- fabs {: d:r :}                     \ d   = |dev - host|
   tol d f< 0= ;                                  \ pass iff d <= tol

\ ---- spawn bin/hb to emit a committed producer, then ptxas-assemble ----------
: ARGV-BASE ( -- )                               \ shared checked PTX-codegen load prefix
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"  >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f" >LEN PROC-ARGV+ ;
: ARGV-V4+ ( -- )                                \ v4 tile vocab for the SAXPY-family producers
   s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+  s" lib/ptx/cg-vec.f" >LEN PROC-ARGV+
   s" lib/ptx/tile.f" >LEN PROC-ARGV+  s" lib/ptx/tile-v4.f" >LEN PROC-ARGV+ ;
: SPAWN-EMIT ( -- )                              \ argv built (entry last) -> capture PTX to PTXTC:PTX$
   ENGINE-CANDIDATE:PATH$ >LEN  CAP-OUT CAP-OUT-CAP >LEN  CAP-ERR CAP-ERR-CAP >LEN  EMIT-TIMEOUT-MS >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   CAP-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD          \ missing producer / nonzero rc -> stderr + throw
   outu LEN>N dup 0= if E-PTX-EMIT throw then EMIT-BYTES !  \ empty PTX -> fail closed
   PTXTC:PTX$ CAP-OUT EMIT-BYTES @ WRITE-ALL ;
: EMIT-GEMM ( -- )   ARGV-BASE  s" tools/ptx/matmul-cg.f"    >LEN PROC-ARGV+  SPAWN-EMIT ;
: EMIT-ATTN ( -- )   ARGV-BASE  s" tools/ptx/attention-cg.f" >LEN PROC-ARGV+  SPAWN-EMIT ;
: EMIT-FUSED ( -- )  ARGV-BASE ARGV-V4+  s" tools/ptx/fused-relu-cg.f" >LEN PROC-ARGV+  SPAWN-EMIT ;
: EMIT-BW ( -- )     ARGV-BASE ARGV-V4+  s" tools/ptx/saxpy-cg.f"       >LEN PROC-ARGV+  SPAWN-EMIT ;
: ASSEMBLE-OK ( -- )                             \ ptxas-assemble PTXTC:PTX$ -> cubin, assert rc 0 (stderr surfaced)
   ASM-OUT CAP-ERR-CAP >LEN  ASM-ERR CAP-ERR-CAP >LEN  PTXTC:ASSEMBLE  PTXTC:ASM-REPORT 0 T= ;

\ ---- CUDA context + module load/unload (shared across the four kernels) ------
create DG-PATH FS-PATH-CAP allot  create DG-KN 32 allot
variable DG-DEV variable DG-CTX variable DG-MOD variable DG-FUNC

: CU-BEGIN ( -- )
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   DG-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   DG-CTX DG-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   DG-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0 ;
: CU-LOAD ( ptr u8 n -- ) {: na:ptr nu:n :}      \ load PTXTC:CUBIN$, resolve function <name> into DG-FUNC
   PTXTC:CUBIN$ DG-PATH >CSTR
   DG-MOD DG-PATH CUDA:CU-MODULE-LOAD CUDA:RC0
   na nu DG-KN >CSTR
   DG-FUNC DG-MOD @ >CUDA-MOD DG-KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0 ;
: CU-END ( -- )
   DG-MOD @ >CUDA-MOD CUDA:CU-MODULE-UNLOAD CUDA:RC0
   DG-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE CUDA:RC0 ;    \ release or bin/hb hangs at exit

\ ============================ SGEMM (MM) golden ==============================
\ A[i][k]=1, B[k][j]=((j&3)+1)*0.25 (independent of k), so C[i][j] = 32*B[0][j]
\ = 8*((j&3)+1): a small KNOWN A*B whose K=32 accumulation is exact in f32.
variable GM-DA variable GM-DB variable GM-DC
variable GM-MV variable GM-NV variable GM-KV
variable GEMM-OK

: GEMM-BVAL ( n -- r )
   3 and dup 0 = if drop 0.25 exit then
   dup 1 = if drop 0.5  exit then
   dup 2 = if drop 0.75 exit then  drop 1.0 ;
: GEMM-REFVAL ( n -- r )  GEMM-BVAL 32.0 f* ;    \ C[i][j] = sum_k 1*B[k][j] = 32*B[0][j]
: FILL-GEMM-A ( -- )  GM-A-ELEMS 0 ?do  1.0 GEMM-A i F!  loop ;
: FILL-GEMM-B ( -- )  GM-B-ELEMS 0 ?do  i GEMM-N mod GEMM-BVAL  GEMM-B i F!  loop ;

: RUN-GEMM ( -- )                                \ launch MM, block 16x16, grid (N/64, M/64); readback -> GEMM-C
   CU-BEGIN  s" MM" CU-LOAD
   GM-DA GM-A-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   GM-DB GM-B-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   GM-DC GM-C-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   GM-DA @ >CUDA-DEVPTR GEMM-A GM-A-BYTES >LEN CUDA:CU-MEMCPY-HTOD CUDA:RC0
   GM-DB @ >CUDA-DEVPTR GEMM-B GM-B-BYTES >LEN CUDA:CU-MEMCPY-HTOD CUDA:RC0
   GM-DC @ >CUDA-DEVPTR PTXSENT:POISON GM-C-ELEMS >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0   \ poison: a dropped write diverges
   GEMM-M GM-MV !  GEMM-N GM-NV !  GEMM-K GM-KV !
   DG-FUNC @ >CUDA-FN 16 16 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   DG-FUNC @ >CUDA-FN GM-PARAM-BYTES >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   DG-FUNC @ >CUDA-FN 0 >IDX  GM-DA 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 8 >IDX  GM-DB 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 16 >IDX GM-DC 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 24 >IDX GM-MV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 28 >IDX GM-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 32 >IDX GM-KV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN GM-GRID-N GM-GRID-M CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   GEMM-C GM-DC @ >CUDA-DEVPTR GM-C-BYTES >LEN CUDA:CU-MEMCPY-DTOH CUDA:RC0
   GM-DA @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   GM-DB @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   GM-DC @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   CU-END ;

: GEMM-MAXERR ( -- r )
   0.0  GM-C-ELEMS 0 ?do
      GEMM-C i F32@ F32>F64   i GEMM-N mod GEMM-REFVAL NARROW  f- fabs  MAXF
   loop ;
: GEMM-ALL-WITHIN? ( -- bool )                   \ every element within atol+rtol (matmul class)
   -1 GEMM-OK !
   GM-C-ELEMS 0 ?do
      GEMM-C i F32@ F32>F64   i GEMM-N mod GEMM-REFVAL NARROW  GM-ATOL GM-RTOL WITHIN? 0= if 0 GEMM-OK ! then
   loop  GEMM-OK @ ;

\ ============================ attention (ATTN) golden =======================
\ K rows all equal -> scores equal per query -> softmax uniform -> O[q] = colmean(V)
\ = [1.0, 1.5] for every q (exp(0)=1 exact, so exp precision drops out).
variable AT-DQ variable AT-DK variable AT-DV variable AT-DO
variable AT-NV variable AT-DPARAM

: ATTN-QVAL ( n n -- r ) {: q:n d:n :}
   d 0 = if
      q 0 = if 0.25 exit then  q 1 = if 0.5 exit then  q 2 = if 0.75 exit then  1.0
   else 0.5 then ;
: ATTN-KVAL ( n n -- r ) {: j:n d:n :}  d 0 = if 1.0 else 0.5 then ;
: ATTN-VVAL ( n n -- r ) {: j:n d:n :}
   d 0 = if
      j 0 = if 1.0 exit then  j 1 = if 0.5 exit then  j 2 = if 2.0 exit then  0.5
   else
      j 0 = if 3.0 exit then  j 1 = if 1.0 exit then  j 2 = if 0.5 exit then  1.5
   then ;
: ATTN-REFVAL ( n n -- r ) {: q:n d:n :}  d 0 = if 1.0 else 1.5 then ;   \ colmean(V)
: FILL-ATTN-Q ( -- )  AT-ELEMS 0 ?do  i ATTN-D /mod swap ATTN-QVAL  ATTN-Q i F!  loop ;
: FILL-ATTN-K ( -- )  AT-ELEMS 0 ?do  i ATTN-D /mod swap ATTN-KVAL  ATTN-K i F!  loop ;
: FILL-ATTN-V ( -- )  AT-ELEMS 0 ?do  i ATTN-D /mod swap ATTN-VVAL  ATTN-V i F!  loop ;

: RUN-ATTN ( -- )                                \ launch ATTN, block N threads, grid N blocks; readback -> ATTN-O
   CU-BEGIN  s" ATTN" CU-LOAD
   AT-DQ AT-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   AT-DK AT-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   AT-DV AT-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   AT-DO AT-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   AT-DQ @ >CUDA-DEVPTR ATTN-Q AT-BYTES >LEN CUDA:CU-MEMCPY-HTOD CUDA:RC0
   AT-DK @ >CUDA-DEVPTR ATTN-K AT-BYTES >LEN CUDA:CU-MEMCPY-HTOD CUDA:RC0
   AT-DV @ >CUDA-DEVPTR ATTN-V AT-BYTES >LEN CUDA:CU-MEMCPY-HTOD CUDA:RC0
   AT-DO @ >CUDA-DEVPTR PTXSENT:POISON AT-ELEMS >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0
   ATTN-N AT-NV !  ATTN-D AT-DPARAM !
   DG-FUNC @ >CUDA-FN ATTN-N 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   DG-FUNC @ >CUDA-FN AT-PARAM-BYTES >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   DG-FUNC @ >CUDA-FN 0 >IDX  AT-DQ 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 8 >IDX  AT-DK 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 16 >IDX AT-DV 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 24 >IDX AT-DO 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 32 >IDX AT-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 36 >IDX AT-DPARAM 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN ATTN-N 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   ATTN-O AT-DO @ >CUDA-DEVPTR AT-BYTES >LEN CUDA:CU-MEMCPY-DTOH CUDA:RC0
   AT-DQ @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   AT-DK @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   AT-DV @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   AT-DO @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   CU-END ;

: ATTN-MAXERR ( -- r )
   0.0  AT-ELEMS 0 ?do
      ATTN-O i F32@ F32>F64   i ATTN-D /mod swap ATTN-REFVAL NARROW  f- fabs  MAXF
   loop ;

\ ==================== SAXPY-family launch (fused + bandwidth) ================
\ Both read x + y and write result to y; params pX(0) pY(8) a(16) n(20) = 24 bytes.
\ A dropped write leaves the input y (which differs from every golden), so the
\ compare is fail-closed without poisoning the read-modify-write output buffer.
variable SP-DX variable SP-DY variable SP-A variable SP-NV

: RUN-SAXPY ( n n n n -- ) {: abits:n block:n grid:n cnt:n :}   \ a-bits, block, grid, active count
   SP-DX SP-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   SP-DY SP-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   SP-DX @ >CUDA-DEVPTR SP-X SP-BYTES >LEN CUDA:CU-MEMCPY-HTOD CUDA:RC0
   SP-DY @ >CUDA-DEVPTR SP-Y SP-BYTES >LEN CUDA:CU-MEMCPY-HTOD CUDA:RC0
   abits SP-A !  cnt SP-NV !
   DG-FUNC @ >CUDA-FN block 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   DG-FUNC @ >CUDA-FN SP-PARAM-BYTES >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   DG-FUNC @ >CUDA-FN 0 >IDX  SP-DX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 8 >IDX  SP-DY 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 16 >IDX SP-A 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN 20 >IDX SP-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   DG-FUNC @ >CUDA-FN grid 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   SP-Y SP-DY @ >CUDA-DEVPTR SP-BYTES >LEN CUDA:CU-MEMCPY-DTOH CUDA:RC0
   SP-DX @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   SP-DY @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0 ;

\ fused relu(a*x+y), a=3: even lane x=2,y=-8 -> 3*2-8=-2 -> relu 0 (clamp exercised);
\ odd lane x=2,y=4 -> 3*2+4=10 -> relu 10 (scale+bias exercised). Dropping RELU makes
\ the even lanes -2; dropping the scale makes the odd lanes 6 - each caught by a lane.
: FU-XVAL ( n -- r )  drop 2.0 ;
: FU-YVAL ( n -- r )  1 and 0 = if -8.0 else 4.0 then ;
: FU-REFVAL ( n -- r )  1 and 0 = if 0.0 else 10.0 then ;
: FILL-FUSED ( -- )  SP-N 0 ?do  i FU-XVAL SP-X i F!  i FU-YVAL SP-Y i F!  loop ;
: FUSED-MAXERR ( -- r )
   0.0  SP-N 0 ?do  SP-Y i F32@ F32>F64  i FU-REFVAL NARROW  f- fabs  MAXF  loop ;

\ bandwidth SAXPY y=a*x+y, a=2, y=0, x[i]=((i&3)+1)*0.25 -> y[i]=2*x[i]: the copy/
\ scale identity the bandwidth kernel must satisfy (right x read, scaled, right store).
: BW-XVAL ( n -- r )  3 and 1+ s>f 4.0 f/ ;      \ {0.25,0.5,0.75,1.0}
: BW-REFVAL ( n -- r )  BW-XVAL 2.0 f* ;         \ 2*x
: FILL-BW ( -- )  SP-N 0 ?do  i BW-XVAL SP-X i F!  0.0 SP-Y i F!  loop ;
: BW-MAXERR ( -- r )
   0.0  SP-N 0 ?do  SP-Y i F32@ F32>F64  i BW-REFVAL NARROW  f- fabs  MAXF  loop ;

\ ---- evidence printing ------------------------------------------------------
: REPORT-ERR ( ptr u8 n r -- ) {: a:ptr u:n e:r :}
   s" device-gold " type  a u type  s" : max|err| vs golden = " type  e 8 F.N cr ;
: REPORT-TOL ( ptr u8 n r r r -- ) {: a:ptr u:n e:r atol:r rtol:r :}
   s" device-gold " type  a u type  s" : max|err| vs golden = " type  e 8 F.N
   s"  (atol " type  atol 8 F.N  s"  rtol " type  rtol 8 F.N  s" )" type cr ;

\ ---- per-kernel golden: emit committed producer -> assemble -> run -> compare -
: GEMM-GOLDEN ( -- )
   EMIT-GEMM  EMIT-BYTES @ 0 > TTRUE  ASSEMBLE-OK
   FILL-GEMM-A  FILL-GEMM-B  RUN-GEMM
   s" SGEMM MM 64x64x32" GEMM-MAXERR GM-ATOL GM-RTOL REPORT-TOL
   s" SGEMM(MM) device==committed golden" T-LABEL  GEMM-ALL-WITHIN? TTRUE ;
: ATTN-GOLDEN ( -- )
   EMIT-ATTN  EMIT-BYTES @ 0 > TTRUE  ASSEMBLE-OK
   FILL-ATTN-Q  FILL-ATTN-K  FILL-ATTN-V  RUN-ATTN
   ATTN-MAXERR {: e:r :}
   s" attention ATTN N=4 D=2" e REPORT-ERR
   s" attention(ATTN) device==committed golden" T-LABEL  e SCALAR-TOL f< TTRUE ;
: FUSED-GOLDEN ( -- )
   EMIT-FUSED  EMIT-BYTES @ 0 > TTRUE  ASSEMBLE-OK
   FILL-FUSED  CU-BEGIN  s" SAXPY" CU-LOAD  FU-A-BITS SP-BLOCK FU-GRID SP-N RUN-SAXPY  CU-END
   FUSED-MAXERR {: e:r :}
   s" fused relu(a*x+y) v4 n=1024" e REPORT-ERR
   s" fused-relu(SAXPY) device==committed golden" T-LABEL  e SCALAR-TOL f< TTRUE ;
: BW-GOLDEN ( -- )
   EMIT-BW  EMIT-BYTES @ 0 > TTRUE  ASSEMBLE-OK
   FILL-BW  CU-BEGIN  s" SAXPY" CU-LOAD  BW-A-BITS SP-BLOCK BW-GRID SP-N RUN-SAXPY  CU-END
   BW-MAXERR {: e:r :}
   s" bandwidth SAXPY scale-identity n=1024" e REPORT-ERR
   s" bandwidth(SAXPY) device==committed golden" T-LABEL  e SCALAR-TOL f< TTRUE ;

\ ---- device-FFI probe (maki/device-smoke.f pattern): libcuda present AND cuInit ok
: DEV? ( -- bool )
   CUDA:OPEN? 0= if false exit then
   0 CUDA:CU-INIT RC>N 0= ;

public

: MAIN ( -- )
   T-RESET
   DEV? 0= if
      s" device-gold: libcuda.so.1 unavailable -> flagship kernel goldens SKIPPED (off-device; file check-loads, emit halves host-proven in device-gold-test.f)" type cr
      T-REPORT exit
   then
   s" device: cuInit OK -> running SGEMM / attention / fused-relu / bandwidth committed goldens on the Orin" type cr
   s" habu-ptx-device-gold" PTXTC:PREPARE
   GEMM-GOLDEN
   ATTN-GOLDEN
   FUSED-GOLDEN
   BW-GOLDEN
   PTXTC:CLEAN
   s" device-gold: all four flagship kernel goldens verified on the Orin" type cr
   T-REPORT ;

MAIN

;package
