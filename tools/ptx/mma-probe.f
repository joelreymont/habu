\ mma-probe.f - single-warp TF32 MMA FRAGMENT-LAYOUT isolation proof (m16n8k8).
\
\ dot habu-tensor-core-mma, task A. The course's #1 device bug is a wrong lane->
\ fragment mapping ("correct in NumPy, garbage on device"), so BEFORE any tiling we
\ prove ONE `mma.sync.aligned.m16n8k8.row.col.f32.tf32.tf32.f32` element-exact vs a
\ host reference. A single warp (32 lanes) computes D[16x8] = A[16x8] . B[8x8] + 0.
\
\ Validated lane->element layout (gid = lane>>2, t = lane&3), the PTX-ISA m16n8k8
\ tf32 fragment (docs/kernel-principles.md), A row-major, B "col" i.e. we index the
\ mathematical B[k][n] directly, C/D row-major:
\   A (16x8), 4 tf32/lane: a0=A[gid][t] a1=A[gid+8][t] a2=A[gid][t+4] a3=A[gid+8][t+4]
\   B (8x8),  2 tf32/lane: b0=B[t][gid] b1=B[t+4][gid]      (cvt.rna.tf32.f32 each)
\   D (16x8), 4 f32/lane:  d0=D[gid][2t] d1=D[gid][2t+1] d2=D[gid+8][2t] d3=D[gid+8][2t+1]
\
\ Element-EXACT design: A[m][k]=m*8+k+1 in 1..128, B[k][n]=k*8+n+1 in 1..64. Every
\ value < 2048 is exact in tf32's 10-bit mantissa and every partial sum < 2^24 is
\ exact in the f32 accumulator, so a correct fragment layout reproduces the integer
\ matmul BIT-EXACT - any permutation/transposition of the mapping mismatches. All
\ values distinct so the test has no accidental symmetry to hide a layout bug.
\
\ Device-only: off the Orin (no libcuda) MP-ALL SKIPS so this file still check-loads.
\ Run on the device (ISOLATED-COPY protocol): bin/hb --load tools/ptx/mma-probe.f

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/array.f
require lib/fs.f
require lib/fs-mutate.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/toolchain.f
require tools/ptx/bench.f

package MMAPROBE

16 constant MP-M   8 constant MP-N   8 constant MP-K
MP-M MP-K * constant MP-AE          \ A elems = 128
MP-K MP-N * constant MP-BE          \ B elems = 64
MP-M MP-N * constant MP-CE          \ C elems = 128

\ ============================ PTX EMIT (trusted target) ======================
: MP-HEAD ( -- )
   s" .visible .entry MMAPROBE(.param .u64 pA, .param .u64 pB, .param .u64 pC)" PTX-L
   s" {" PTX-L
   s" .reg .f32 %f<32>;" PTX-L
   s" .reg .b32 %r<32>;" PTX-L
   s" .reg .b64 %rd<16>;" PTX-L ;

: MP-PARAMS ( -- )
   s" ld.param.u64 %rd1, [pA];" PTX-L
   s" ld.param.u64 %rd2, [pB];" PTX-L
   s" ld.param.u64 %rd3, [pC];" PTX-L
   s" cvta.to.global.u64 %rd1, %rd1;" PTX-L
   s" cvta.to.global.u64 %rd2, %rd2;" PTX-L
   s" cvta.to.global.u64 %rd3, %rd3;" PTX-L ;

\ lane -> (gid,t) and the reused row/col bases: %r4=gid*8 %r5=(gid+8)*8 %r7=t*8 %r8=2t
: MP-LANE ( -- )
   s" mov.u32 %r1, %tid.x;" PTX-L
   s" shr.u32 %r2, %r1, 2;" PTX-L        \ gid = lane>>2
   s" and.b32 %r3, %r1, 3;" PTX-L        \ t   = lane&3
   s" mul.lo.u32 %r4, %r2, 8;" PTX-L     \ gid*8      (A/D row gid, stride N=8)
   s" add.u32 %r5, %r4, 64;" PTX-L       \ (gid+8)*8  (A/D row gid+8)
   s" mul.lo.u32 %r7, %r3, 8;" PTX-L     \ t*8        (B row t, stride N=8)
   s" shl.b32 %r8, %r3, 1;" PTX-L ;      \ 2t         (D col base)

\ load A[row][col] from %rd1 into f32 %f1, then cvt.rna.tf32 -> named tf32 reg; index in %r6
: MP-LOAD-A ( -- )
   s" add.u32 %r6, %r4, %r3;" PTX-L                 \ a0 = A[gid][t]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd11, %rd1, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L  s" cvt.rna.tf32.f32 %r10, %f1;" PTX-L
   s" add.u32 %r6, %r5, %r3;" PTX-L                 \ a1 = A[gid+8][t]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd11, %rd1, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L  s" cvt.rna.tf32.f32 %r11, %f1;" PTX-L
   s" add.u32 %r6, %r4, %r3;" PTX-L  s" add.u32 %r6, %r6, 4;" PTX-L   \ a2 = A[gid][t+4]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd11, %rd1, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L  s" cvt.rna.tf32.f32 %r12, %f1;" PTX-L
   s" add.u32 %r6, %r5, %r3;" PTX-L  s" add.u32 %r6, %r6, 4;" PTX-L   \ a3 = A[gid+8][t+4]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd11, %rd1, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L  s" cvt.rna.tf32.f32 %r13, %f1;" PTX-L ;

\ load B[row][col] from %rd2 (row-major KxN), 2 tf32/lane
: MP-LOAD-B ( -- )
   s" add.u32 %r6, %r7, %r2;" PTX-L                 \ b0 = B[t][gid]  idx = t*8 + gid
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd11, %rd2, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L  s" cvt.rna.tf32.f32 %r14, %f1;" PTX-L
   s" add.u32 %r6, %r7, %r2;" PTX-L  s" add.u32 %r6, %r6, 32;" PTX-L  \ b1 = B[t+4][gid]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd11, %rd2, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L  s" cvt.rna.tf32.f32 %r15, %f1;" PTX-L ;

: MP-MMA ( -- )
   s" mov.f32 %f10, 0f00000000;" PTX-L  s" mov.f32 %f11, 0f00000000;" PTX-L
   s" mov.f32 %f12, 0f00000000;" PTX-L  s" mov.f32 %f13, 0f00000000;" PTX-L
   s" mma.sync.aligned.m16n8k8.row.col.f32.tf32.tf32.f32 {%f20, %f21, %f22, %f23}, {%r10, %r11, %r12, %r13}, {%r14, %r15}, {%f10, %f11, %f12, %f13};" PTX-L ;

\ store the 4 D accumulators to C[row][col] (row-major 16x8)
: MP-STORE-D ( -- )
   s" add.u32 %r6, %r4, %r8;" PTX-L                 \ d0 = D[gid][2t]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd12, %rd3, %rd10;" PTX-L
   s" st.global.f32 [%rd12], %f20;" PTX-L
   s" add.u32 %r6, %r6, 1;" PTX-L                   \ d1 = D[gid][2t+1]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd12, %rd3, %rd10;" PTX-L
   s" st.global.f32 [%rd12], %f21;" PTX-L
   s" add.u32 %r6, %r5, %r8;" PTX-L                 \ d2 = D[gid+8][2t]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd12, %rd3, %rd10;" PTX-L
   s" st.global.f32 [%rd12], %f22;" PTX-L
   s" add.u32 %r6, %r6, 1;" PTX-L                   \ d3 = D[gid+8][2t+1]
   s" mul.wide.u32 %rd10, %r6, 4;" PTX-L  s" add.u64 %rd12, %rd3, %rd10;" PTX-L
   s" st.global.f32 [%rd12], %f23;" PTX-L ;

: EMIT-MMA-PROBE ( -- )
   PTX-HEADER-SM87  PTX-NL
   MP-HEAD  MP-PARAMS  MP-LANE
   MP-LOAD-A  MP-LOAD-B  MP-MMA  MP-STORE-D
   s" ret;" PTX-L  s" }" PTX-L ;

\ ============================ HOST reference + device run =====================
create MP-HA MP-AE cells allot      \ host A (f64 cells)
create MP-HB MP-BE cells allot      \ host B
create MP-HREF MP-CE cells allot    \ host reference C (f64)
create MP-HC MP-CE cells allot      \ device C read back (f64)
create MP-PA MP-AE 4 * allot        \ packed f32 A
create MP-PB MP-BE 4 * allot        \ packed f32 B
create MP-PC MP-CE 4 * allot        \ packed f32 C
create MP-QO $1000 allot  create MP-QE $1000 allot
variable MP-DA  variable MP-DB  variable MP-DC
variable MP-BADI
create MP-MAXERR 1 cells allot      \ holds one f64 max abs error

: MP-FILL-HOST ( -- )               \ distinct exact-in-tf32 values
   MP-AE 0 ?do  i 1+ s>f  MP-HA i T-SET  loop
   MP-BE 0 ?do  i 1+ s>f  MP-HB i T-SET  loop ;

: MP-DOT ( n n -- r ) {: m:n col:n :}   \ sum_k A[m][k]*B[k][col]
   0.0
   MP-K 0 ?do
      MP-HA m MP-K * i + T-GET
      MP-HB i MP-N * col + T-GET
      f* f+
   loop ;
: MP-REF ( -- )
   MP-M 0 ?do  i {: m:n :}
      MP-N 0 ?do  m i MP-DOT  MP-HREF m MP-N * i + T-SET  loop
   loop ;

: MP-ASSEMBLE ( -- )
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   MP-QO $1000 >LEN MP-QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT {: rc:n :}
   rc 0= 0= if s" mma-probe: ptxas failed" 1 die then ;

: MP-RUN-DEVICE ( -- )
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" MMAPROBE" PTXBENCH:KERNEL!  s" MMAPROBE" PTXBENCH:LABEL!
   PTXBENCH:OPEN  PTXBENCH:LOAD
   MP-AE 4 * MP-DA PTXBENCH:DEVICE-ALLOC
   MP-BE 4 * MP-DB PTXBENCH:DEVICE-ALLOC
   MP-CE 4 * MP-DC PTXBENCH:DEVICE-ALLOC
   MP-HA MP-AE MP-PA F32-PACK
   MP-HB MP-BE MP-PB F32-PACK
   MP-DA @ MP-PA MP-AE 4 * PTXBENCH:HTOD
   MP-DB @ MP-PB MP-BE 4 * PTXBENCH:HTOD
   32 PTXBENCH:BLOCK!  1 PTXBENCH:BLOCKY!
   1  PTXBENCH:GRID!   1 PTXBENCH:GRIDY!
   24 PTXBENCH:PARAM-BYTES!
   PTXBENCH:PREPARE-LAUNCH
   0  MP-DA PTXBENCH:PARAM-PTR!
   8  MP-DB PTXBENCH:PARAM-PTR!
   16 MP-DC PTXBENCH:PARAM-PTR!
   PTXBENCH:LAUNCH  PTXBENCH:SYNC
   MP-PC MP-DC @ MP-CE 4 * PTXBENCH:DTOH
   MP-PC MP-CE MP-HC F32-UNPACK
   MP-DA @ PTXBENCH:DEVICE-FREE
   MP-DB @ PTXBENCH:DEVICE-FREE
   MP-DC @ PTXBENCH:DEVICE-FREE
   PTXBENCH:UNLOAD  PTXBENCH:CLOSE ;

: MP-COMPARE ( -- n )               \ mismatch count; sets MP-BADI, MP-MAXERR
   -1 MP-BADI !  0.0 MP-MAXERR !  0
   MP-CE 0 ?do
      MP-HC i T-GET  MP-HREF i T-GET  f-  fabs {: err:r :}
      err 0.0 f> if
         1+  MP-BADI @ 0 < if i MP-BADI ! then
         err MP-MAXERR @ f> if err MP-MAXERR ! then
      then
   loop ;

: MP-REPORT ( n -- ) {: bad:n :}
   s" == MMA fragment-isolation probe: m16n8k8 tf32, single warp ==" type cr
   s" C[0][0]="   type MP-HC 0 T-GET f>s . s"  ref=" type MP-HREF 0 T-GET f>s . cr
   s" C[15][7]="  type MP-HC MP-CE 1- T-GET f>s . s"  ref=" type MP-HREF MP-CE 1- T-GET f>s . cr
   s" C[8][3]="   type MP-HC 8 MP-N * 3 + T-GET f>s . s"  ref=" type MP-HREF 8 MP-N * 3 + T-GET f>s . cr
   s" mismatches=" type bad . s"  of " type MP-CE . s"  elems" type cr
   bad 0= if s" RESULT: PASS (element-exact, fragment layout validated)" type cr
   else s" RESULT: FAIL first-bad-elem=" type MP-BADI @ . s"  maxerr=" type MP-MAXERR @ f>s . cr then ;

public
: MP-EMIT ( -- )  EMIT-MMA-PROBE ;      \ dump the probe PTX to stdout (inspection)
: MP-ALL ( -- )
   CUDA:OPEN? 0= if s" mma-probe: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   s" habu-mma-probe" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MMA-PROBE  PTX-CAPTURE-OFF
   MP-ASSEMBLE
   MP-FILL-HOST  MP-REF
   MP-RUN-DEVICE
   MP-COMPARE MP-REPORT
   PTXTC:CLEAN ;

end-package

MMAPROBE:MP-ALL
