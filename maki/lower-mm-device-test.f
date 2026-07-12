\ maki/lower-mm-device-test.f - the Orin device leg for the matmul/linear region lowering.
\
\ CAD-PLAN section 11 device-vs-host GOLDEN, slice 3. For each matmul model (MATMUL,
\ LINEAR+bias, and the LINEAR->GELU epilogue-fusion case) it builds the model, fuses it, and
\ lowers region 0 onto a REGION_0 tiled-GEMM PTX kernel: spawn a fresh bin/hb (via
\ LOWER-DRIVER!) to EMIT the kernel to PTXTC:PTX$, EMIT-GUARD, ptxas ASSEMBLE + ASM-REPORT,
\ then LOWER-GOLDEN (maki/lower-golden.f) runs the host executor and the device kernel on the
\ same synthetic inputs and compares under the matmul tolerance (rtol 1e-4, ACC-F32 over
\ K<=256). Off the Orin (no libcuda) the device leg is reported SKIPPED and the host build
\ still runs.
\
\ Each MODEL: line runs at load time (a parse-time IR build, like the slice-1/2 device
\ tests); LMD-GOLD1 lowers + launches whatever IR is current, so the same model text feeds
\ the host reference, the launch staging, and the spawned child that emits the kernel PTX.
\ Not part of the maki gate (maki/test.f) - it needs the CUDA toolkit + a device. Run on the
\ Orin: scp to zed:Work/habu then `bin/hb --load maki/lower-mm-device-test.f`.

require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/fs.f
require lib/fs-mutate.f
require lib/float.f
require lib/fmt.f
require lib/ptx/toolchain.f
require maki/device-artifacts.f
require maki/cad.f
require maki/lower-golden.f

package MAKI

create LMD-OUT $10000 allot  create LMD-ERR $1000 allot   \ blocked-tile PTX is ~28 KB (64 KB headroom)
create LMD-QO  $1000 allot  create LMD-QE  $2000 allot

\ ---- spawn bin/hb to emit region 0's PTX into PTXTC:PTX$ (child re-builds the IR) --
: LMD-EMIT ( ptr u8 n -- ) {: sa:ptr su:n :}
   sa su  s" require maki/lower-mm.f"  s" LMM-EMIT"  0  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   s" bin/hb" >LEN  LMD-OUT $10000 >LEN  LMD-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   LMD-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ surface stderr + throw on a nonzero child
   PTXTC:PTX$ LMD-OUT outu LEN>N WRITE-ALL ;

: LMD-PTXAS ( -- n )  LMD-QO $1000 >LEN LMD-QE $2000 >LEN PTXTC:ASSEMBLE ;

\ ---- per-element evidence (device value | host value rounded to f32) --------
: LMD-FP ( r -- )  SB-RESET 6 SB-FIX SB$ type ;
: LMD-HOST@ ( n -- r )  LLA-OUT-NODE@ EX-OUT@ swap T-GET  F64>F32 F32>F64 ;   \ narrowed host elem
: LMD-EVIDENCE ( -- )
   s" elem   device        host_f32" type cr
   LLA-ELEMS@ 0 ?do
      s"   " type i SB-RESET SB-INT SB$ type s"    " type
      i LLA-OUT@ LMD-FP  s"    " type  i LMD-HOST@ LMD-FP  cr
   loop ;

\ ---- one model: emit -> assemble -> golden -> evidence -> assert V-PASS ------
\ Skips (no emit/ptxas) off-device, so the file still loads where there is no CUDA.
: LMD-GOLD1 ( ptr u8 n -- ) {: sa:ptr su:n :}
   CUDA:OPEN? 0= if exit then
   sa su LMD-EMIT
   LMD-PTXAS PTXTC:ASM-REPORT 0 T=                        \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LLA-CUBIN!
   0 LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   LMD-EVIDENCE
   v V-PASS T= ;

\ ---- capped evidence (first k + last k) for the large blocked shapes (4096 elems) ----------
: LMD-EVIDENCE-ROW ( n -- )  {: i:n :}
   s"   " type i SB-RESET SB-INT SB$ type s"    " type
   i LLA-OUT@ LMD-FP  s"    " type  i LMD-HOST@ LMD-FP  cr ;
: LMD-EVIDENCE-CAP ( n -- ) {: k:n :}
   s" elem   device        host_f32" type cr
   LLA-ELEMS@ {: e:n :}
   e 2 k * <= if  e 0 ?do i LMD-EVIDENCE-ROW loop  exit then    \ small enough: full dump
   k 0 ?do i LMD-EVIDENCE-ROW loop
   s"   ... (" type e 2 k * - SB-RESET SB-INT SB$ type s"  elems elided) ..." type cr
   e e k - ?do i LMD-EVIDENCE-ROW loop ;

\ like LMD-GOLD1 but with capped per-element evidence (the 64x64 tile has 4096 elements)
: LMD-GOLD1-BIG ( ptr u8 n -- ) {: sa:ptr su:n :}
   CUDA:OPEN? 0= if exit then
   sa su LMD-EMIT
   LMD-PTXAS PTXTC:ASM-REPORT 0 T=
   PTXTC:CUBIN$ LLA-CUBIN!
   0 LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   8 LMD-EVIDENCE-CAP
   v V-PASS T= ;

: LMD-BEGIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" lower-mm-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      exit then
   s" habu-lmm-drv" MAKI-GRADE:PREPARE
   s" habu-lmm-ptx" PTXTC:PREPARE ;

: LMD-END ( -- )
   CUDA:OPEN? 0= if  0 0= TTRUE  T-REPORT exit then
   PTXTC:CLEAN  MAKI-GRADE:CLEAN
   T-REPORT ;

;package

package MAKI
LMD-BEGIN

s" == MATMUL 8x8@8x8 ==" type cr
MODEL: MM ( x:8x8 w:8x8 -- y ) MATMUL ;  FP-BUILD
s" MODEL: MM ( x:8x8 w:8x8 -- y ) MATMUL ;" LMD-GOLD1

s" == LINEAR 4x8@8x16 + bias ==" type cr
MODEL: LN ( x:4x8 w:8x16 b:1x16 -- y ) LINEAR ;  FP-BUILD
s" MODEL: LN ( x:4x8 w:8x16 b:1x16 -- y ) LINEAR ;" LMD-GOLD1

s" == LINEAR GELU 4x8@8x16 (epilogue fusion) ==" type cr
MODEL: LG ( x:4x8 w:8x16 b:1x16 -- y ) LINEAR GELU ;  FP-BUILD
s" MODEL: LG ( x:4x8 w:8x16 b:1x16 -- y ) LINEAR GELU ;" LMD-GOLD1

\ ---- large shapes exercise the REGISTER-BLOCKED tile (64x64 output, shared staging, 4x4 acc) --
s" == BLOCKED MATMUL 64x64@64x64 (register-blocked tile) ==" type cr
MODEL: MB ( x:64x64 w:64x64 -- y ) MATMUL ;  FP-BUILD
s" MODEL: MB ( x:64x64 w:64x64 -- y ) MATMUL ;" LMD-GOLD1-BIG

s" == BLOCKED LINEAR GELU 64x64@64x64 (bias + gelu fused into the blocked write) ==" type cr
MODEL: LGB ( x:64x64 w:64x64 b:1x64 -- y ) LINEAR GELU ;  FP-BUILD
s" MODEL: LGB ( x:64x64 w:64x64 b:1x64 -- y ) LINEAR GELU ;" LMD-GOLD1-BIG

LMD-END
;package
