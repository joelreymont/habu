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
require lib/float32-buffer.f
require lib/fmt.f
require maki/array.f
require lib/fs.f
require lib/fs-mutate.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/toolchain.f
require maki/eval/active-target.f
require tools/ptx/bench.f

package MMAPROBE

using F32-BUF

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
   PTX-HEADER  PTX-NL
   MP-HEAD  MP-PARAMS  MP-LANE
   MP-LOAD-A  MP-LOAD-B  MP-MMA  MP-STORE-D
   s" ret;" PTX-L  s" }" PTX-L ;

\ ============================ ldmatrix FRAGMENT-LOAD variant ==================
\ Same m16n8k8 tf32 tile, but the A(16x8) fragment is loaded from .shared with ONE
\ ldmatrix.sync.aligned.m8n8.x4.shared.b16 (the cg-mma.f rung-1 target) and B(8x8)
\ with raw ld.shared.b32. Isolation proof that the ldmatrix lane->fragment mapping is
\ element-exact BEFORE the K-looping kernel uses it.
\
\ tf32 via ldmatrix.b16: a tf32 value is 32 bits = 2 adjacent b16 halves, so an
\ 8-row x 4-tf32-col tile IS one 8x8 b16 tile; the 16x8 A fragment = 4 such tiles =
\ one ldmatrix.x4, whose result regs map to {a0,a1,a2,a3} = {A[gid][t],A[gid+8][t],
\ A[gid][t+4],A[gid+8][t+4]} exactly. No cvt: the raw f32 bits land in the .b32 reg
\ and mma.sync.tf32 reads the top 19 bits (round-toward-zero). Integer operands are
\ exact in tf32's 10-bit mantissa, so element-exactness holds identically to MP-ALL;
\ this is the committed correctness check for the ldmatrix load mechanism.
768 constant MP-LDM-SHB           \ shared bytes: A 16x8 f32 (512) + B 8x8 f32 (256)
512 constant MP-LDM-BOFF          \ B tile byte offset within SHM

: MP-LDM-HEAD ( -- )
   s" .visible .entry MMALDM(.param .u64 pA, .param .u64 pB, .param .u64 pC)" PTX-L
   s" {" PTX-L
   s" .reg .f32 %f<32>;" PTX-L
   s" .reg .b32 %r<48>;" PTX-L
   s" .reg .b64 %rd<16>;" PTX-L
   SB-RESET s" .shared .align 16 .b8 SHM[" SB-APPEND MP-LDM-SHB FMT:SB-U s" ];" SB-APPEND SB$ PTX-L ;

\ cooperative global->shared staging (single warp): A 4 elems/lane, B 2/lane, raw b32 copy.
: MP-LDM-STAGE ( -- )
   s" mov.u32 %r1, %tid.x;" PTX-L
   s" mov.u32 %r7, SHM;" PTX-L
   4 0 do                                        \ A[c], c = lane + m*32
      SB-RESET s" add.u32 %r20, %r1, " SB-APPEND i 32 * FMT:SB-U s" ;" SB-APPEND SB$ PTX-L
      s" mul.wide.u32 %rd10, %r20, 4;" PTX-L  s" add.u64 %rd11, %rd1, %rd10;" PTX-L
      s" ld.global.b32 %r21, [%rd11];" PTX-L
      s" shl.b32 %r22, %r20, 2;" PTX-L  s" add.u32 %r22, %r7, %r22;" PTX-L
      s" st.shared.b32 [%r22], %r21;" PTX-L
   loop
   2 0 do                                        \ B[c], c = lane + m*32
      SB-RESET s" add.u32 %r20, %r1, " SB-APPEND i 32 * FMT:SB-U s" ;" SB-APPEND SB$ PTX-L
      s" mul.wide.u32 %rd10, %r20, 4;" PTX-L  s" add.u64 %rd11, %rd2, %rd10;" PTX-L
      s" ld.global.b32 %r21, [%rd11];" PTX-L
      s" shl.b32 %r22, %r20, 2;" PTX-L
      SB-RESET s" add.u32 %r22, %r22, " SB-APPEND MP-LDM-BOFF FMT:SB-U s" ;" SB-APPEND SB$ PTX-L
      s" add.u32 %r22, %r7, %r22;" PTX-L
      s" st.shared.b32 [%r22], %r21;" PTX-L
   loop
   s" bar.sync 0;" PTX-L ;

\ ldmatrix.x4 A: lane provides row addr of tile (lane>>3) row (lane&7); each lane then
\ receives {a0,a1,a2,a3}. row = (tsel&1)*8 + rt, kcol = (tsel>>1)*4 (SH_A row stride 8f=32B).
: MP-LDM-LOAD-A ( -- )
   s" mov.u32 %r1, %tid.x;" PTX-L
   s" and.b32 %r2, %r1, 7;" PTX-L                \ rt   = lane&7
   s" shr.u32 %r3, %r1, 3;" PTX-L                \ tsel = lane>>3
   s" and.b32 %r4, %r3, 1;" PTX-L  s" shl.b32 %r4, %r4, 3;" PTX-L   \ (tsel&1)*8
   s" add.u32 %r5, %r4, %r2;" PTX-L              \ row
   s" mul.lo.u32 %r5, %r5, 32;" PTX-L            \ row*32 (row byte base)
   s" shr.u32 %r6, %r3, 1;" PTX-L  s" shl.b32 %r6, %r6, 4;" PTX-L   \ (tsel>>1)*16 (kcol bytes)
   s" add.u32 %r5, %r5, %r6;" PTX-L
   s" mov.u32 %r7, SHM;" PTX-L  s" add.u32 %r5, %r7, %r5;" PTX-L
   s" ldmatrix.sync.aligned.m8n8.x4.shared.b16 {%r10,%r11,%r12,%r13}, [%r5];" PTX-L ;

\ raw b32 B fragment: b0=B[t][gid], b1=B[t+4][gid] (no cvt; mma truncates f32->tf32)
: MP-LDM-LOAD-B ( -- )
   s" mov.u32 %r1, %tid.x;" PTX-L
   s" shr.u32 %r2, %r1, 2;" PTX-L                \ gid = lane>>2
   s" and.b32 %r3, %r1, 3;" PTX-L                \ t   = lane&3
   s" mov.u32 %r7, SHM;" PTX-L
   s" mul.lo.u32 %r4, %r3, 8;" PTX-L  s" add.u32 %r4, %r4, %r2;" PTX-L  \ t*8+gid
   s" shl.b32 %r4, %r4, 2;" PTX-L
   SB-RESET s" add.u32 %r4, %r4, " SB-APPEND MP-LDM-BOFF FMT:SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r4, %r7, %r4;" PTX-L
   s" ld.shared.b32 %r14, [%r4];" PTX-L
   s" add.u32 %r5, %r3, 4;" PTX-L  s" mul.lo.u32 %r5, %r5, 8;" PTX-L  s" add.u32 %r5, %r5, %r2;" PTX-L  \ (t+4)*8+gid
   s" shl.b32 %r5, %r5, 2;" PTX-L
   SB-RESET s" add.u32 %r5, %r5, " SB-APPEND MP-LDM-BOFF FMT:SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r5, %r7, %r5;" PTX-L
   s" ld.shared.b32 %r15, [%r5];" PTX-L ;

: EMIT-MMA-LDM-PROBE ( -- )
   PTX-HEADER  PTX-NL
   MP-LDM-HEAD  MP-PARAMS  MP-LDM-STAGE
   MP-LDM-LOAD-A  MP-LDM-LOAD-B                   \ operands -> %r10..15
   MP-LANE  MP-MMA  MP-STORE-D                    \ MP-LANE re-derives the D-store geometry (%r4,%r5,%r8)
   s" ret;" PTX-L  s" }" PTX-L ;

\ ============================ B-side ldmatrix FRAGMENT-LOAD variant (dot habu-mma-wave-2) ==========
\ The wave-2 lever (dot): replace the warp's scalar B loads with ONE ldmatrix from a TRANSPOSED Bs.
\ This is NEW geometry - the top cause of "correct in NumPy, garbage on device" - so it is proven
\ element-exact in isolation HERE before any kernel use (feeds habu-ship-swizzled-mma).
\
\ WHY TRANSPOSED Bs. The device-proven B fragment is b0=B[t][gid], b1=B[t+4][gid] (gid=lane>>2,
\ t=lane&3). ldmatrix.m8n8 (non-trans) returns, for each loaded 8x8-b16 tile, reg = tile[row=lane>>2]
\ [tf32col=lane&3] (the SAME result law the A-fragment ldmatrix.x4 proof pins). So a tile T with
\ T[lane>>2][lane&3] = B[lane&3][lane>>2] must be the TRANSPOSE of B: stage SHM_BT[n][k] = B[k][n]
\ (n-major, k contiguous, row stride 8 tf32 = 32 B). Then one ldmatrix.x2 loads the two b16 tiles
\ (k-cols 0-3 and 4-7): reg0 = SHM_BT[gid][t] = B[t][gid] = b0, reg1 = SHM_BT[gid][t+4] = B[t+4][gid]
\ = b1 - exactly {b0,b1}. ldmatrix.TRANS is NOT usable: it transposes at b16 granularity and a tf32 is
\ two b16 halves, so .trans would split every tf32 - the transpose must be in the STAGING, not the load.
\ A is loaded the proven global scalar+cvt way (MP-LOAD-A) so this isolates ONLY the B mapping.
256 constant MP-BLDM-SHB          \ shared bytes: B transposed, 8x8 tf32 (SHM_BT[n][k], row stride 32 B)

: MP-BLDM-HEAD ( -- )
   s" .visible .entry MMABLDM(.param .u64 pA, .param .u64 pB, .param .u64 pC)" PTX-L
   s" {" PTX-L
   s" .reg .f32 %f<32>;" PTX-L
   s" .reg .b32 %r<48>;" PTX-L
   s" .reg .b64 %rd<16>;" PTX-L
   SB-RESET s" .shared .align 16 .b8 SHM[" SB-APPEND MP-BLDM-SHB FMT:SB-U s" ];" SB-APPEND SB$ PTX-L ;

\ cooperative global->shared TRANSPOSED staging (single warp, 2 elems/lane): SHM_BT[n][k] = B[k][n].
: MP-BLDM-STAGE-BT ( -- )
   s" mov.u32 %r1, %tid.x;" PTX-L
   s" mov.u32 %r7, SHM;" PTX-L
   2 0 do                                        \ e = lane + m*32 ; n = e>>3, k = e&7
      SB-RESET s" add.u32 %r20, %r1, " SB-APPEND i 32 * FMT:SB-U s" ;" SB-APPEND SB$ PTX-L
      s" shr.u32 %r21, %r20, 3;" PTX-L           \ n = e>>3
      s" and.b32 %r22, %r20, 7;" PTX-L           \ k = e&7
      s" shl.b32 %r23, %r22, 3;" PTX-L           \ global src B[k][n] = k*8 + n
      s" add.u32 %r23, %r23, %r21;" PTX-L
      s" mul.wide.u32 %rd10, %r23, 4;" PTX-L  s" add.u64 %rd11, %rd2, %rd10;" PTX-L
      s" ld.global.b32 %r24, [%rd11];" PTX-L
      s" shl.b32 %r25, %r21, 5;" PTX-L           \ shared dst SHM + n*32 + k*4
      s" shl.b32 %r23, %r22, 2;" PTX-L
      s" add.u32 %r25, %r25, %r23;" PTX-L
      s" add.u32 %r25, %r7, %r25;" PTX-L
      s" st.shared.b32 [%r25], %r24;" PTX-L
   loop
   s" bar.sync 0;" PTX-L ;

\ ldmatrix.x2 from SHM_BT: tile p (=lane>>3, clamped to {0,1}) row r (=lane&7), addr = SHM + r*32 + p*16.
\ reg0 = b0 = B[t][gid], reg1 = b1 = B[t+4][gid] -> the mma B operand {%r14,%r15}.
: MP-BLDM-LOAD-B ( -- )
   s" mov.u32 %r1, %tid.x;" PTX-L
   s" mov.u32 %r7, SHM;" PTX-L
   s" and.b32 %r2, %r1, 7;" PTX-L                \ r = lane&7 (row within the 8x8 tile)
   s" shr.u32 %r3, %r1, 3;" PTX-L                \ p = lane>>3
   s" and.b32 %r3, %r3, 1;" PTX-L                \ clamp to {0,1} (lanes 16-31 addr ignored; keep it in-range)
   s" shl.b32 %r4, %r2, 5;" PTX-L                \ r*32
   s" shl.b32 %r5, %r3, 4;" PTX-L                \ p*16 (k-hi half offset)
   s" add.u32 %r4, %r4, %r5;" PTX-L
   s" add.u32 %r4, %r7, %r4;" PTX-L
   s" ldmatrix.sync.aligned.m8n8.x2.shared.b16 {%r14,%r15}, [%r4];" PTX-L ;

: EMIT-MMA-BLDM-PROBE ( -- )
   PTX-HEADER  PTX-NL
   MP-BLDM-HEAD  MP-PARAMS  MP-BLDM-STAGE-BT
   MP-BLDM-LOAD-B                                 \ B operand -> %r14,%r15
   MP-LANE  MP-LOAD-A                             \ A operand (global scalar+cvt, proven) -> %r10..13, D geometry
   MP-MMA  MP-STORE-D
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
   ATGT:LABEL$ PTXTC:TC-ARCH!             \ assembler arch from the probed active target (else PTXTC:ASSEMBLE fails closed on the GB10)
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   MP-QO $1000 >LEN MP-QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT {: rc:n :}
   rc 0= 0= if s" mma-probe: ptxas failed" 1 die then ;

\ One owning frame around the probe: the scope owns ctx + module + the three device buffers
\ (transferred as acquired) and unwinds them on return OR throw - no open-coded FREE/UNLOAD/CLOSE.
: MP-RUN-DEVICE ( ptr u8 n -- ) {: ka:ptr ku:n :}
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   ka ku PTXBENCH:KERNEL!  ka ku PTXBENCH:LABEL!
   [: PTXBENCH:OPEN  PTXBENCH:OWN-CTX   PTXBENCH:LOAD  PTXBENCH:OWN-MOD
      MP-AE 4 * MP-DA PTXBENCH:DEVICE-ALLOC  MP-DA PTXBENCH:OWN-DEV
      MP-BE 4 * MP-DB PTXBENCH:DEVICE-ALLOC  MP-DB PTXBENCH:OWN-DEV
      MP-CE 4 * MP-DC PTXBENCH:DEVICE-ALLOC  MP-DC PTXBENCH:OWN-DEV
      MP-HA MP-AE MP-PA PACK
      MP-HB MP-BE MP-PB PACK
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
      MP-PC MP-CE MP-HC UNPACK ;] CUDA-SCOPE:SCOPE ;

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
   s" C[0][0]="   type MP-HC 0 T-GET f>s . s"  ref=" type MP-HREF 0 T-GET f>s . cr
   s" C[15][7]="  type MP-HC MP-CE 1- T-GET f>s . s"  ref=" type MP-HREF MP-CE 1- T-GET f>s . cr
   s" C[8][3]="   type MP-HC 8 MP-N * 3 + T-GET f>s . s"  ref=" type MP-HREF 8 MP-N * 3 + T-GET f>s . cr
   s" mismatches=" type bad . s"  of " type MP-CE . s"  elems" type cr
   bad 0= if s" RESULT: PASS (element-exact, fragment layout validated)" type cr
   else s" RESULT: FAIL first-bad-elem=" type MP-BADI @ . s"  maxerr=" type MP-MAXERR @ f>s . cr then ;

public
: MP-EMIT ( -- )  EMIT-MMA-PROBE ;          \ dump the scalar probe PTX to stdout (inspection)
: MP-LDM-EMIT ( -- )  EMIT-MMA-LDM-PROBE ;  \ dump the ldmatrix probe PTX to stdout (inspection)
: MP-BLDM-EMIT ( -- )  EMIT-MMA-BLDM-PROBE ;  \ dump the B-side ldmatrix probe PTX to stdout (inspection)
: MP-ALL ( -- )
   CUDA:OPEN? 0= if s" mma-probe: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   s" habu-mma-probe" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MMA-PROBE  PTX-CAPTURE-OFF
   MP-ASSEMBLE
   MP-FILL-HOST  MP-REF
   s" MMAPROBE" MP-RUN-DEVICE
   s" == MMA fragment-isolation probe: m16n8k8 tf32, single warp (scalar+cvt) ==" type cr
   MP-COMPARE MP-REPORT
   PTXTC:CLEAN ;

: MP-LDM-ALL ( -- )
   CUDA:OPEN? 0= if s" mma-probe: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   s" habu-mma-ldm-probe" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MMA-LDM-PROBE  PTX-CAPTURE-OFF
   MP-ASSEMBLE
   MP-FILL-HOST  MP-REF
   s" MMALDM" MP-RUN-DEVICE
   s" == MMA fragment-isolation probe: m16n8k8 tf32, single warp (ldmatrix.x4 A + raw b32 B) ==" type cr
   MP-COMPARE MP-REPORT
   PTXTC:CLEAN ;

: MP-BLDM-ALL ( -- )
   CUDA:OPEN? 0= if s" mma-probe: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   s" habu-mma-bldm-probe" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MMA-BLDM-PROBE  PTX-CAPTURE-OFF
   MP-ASSEMBLE
   MP-FILL-HOST  MP-REF
   s" MMABLDM" MP-RUN-DEVICE
   s" == MMA fragment-isolation probe: m16n8k8 tf32, single warp (A scalar+cvt + B ldmatrix.x2 transposed-Bs) ==" type cr
   MP-COMPARE MP-REPORT
   PTXTC:CLEAN ;

;using
;package

MMAPROBE:MP-ALL
MMAPROBE:MP-LDM-ALL
MMAPROBE:MP-BLDM-ALL
