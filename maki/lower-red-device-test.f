\ maki/lower-red-device-test.f - the Orin device leg for the row-reduce region lowering.
\
\ CAD-PLAN section 11 device-vs-host GOLDEN, slice 2. For each row-reduce model (RMSNORM,
\ LAYERNORM, SOFTMAX-ROW, and the GELU->RMSNORM prologue-fusion case) it builds the model,
\ fuses it, and lowers region 0 onto a REGION_0 block-per-row PTX kernel: spawn a fresh
\ bin/hb (via LOWER-DRIVER!) to EMIT the kernel to PTXTC:PTX$, EMIT-GUARD, ptxas ASSEMBLE +
\ ASM-REPORT, then LOWER-GOLDEN (maki/lower-golden.f) runs the host executor and the device
\ kernel on the same synthetic inputs and compares under the reduction tolerance. Off the
\ Orin (no libcuda) the device leg is reported SKIPPED and the host build still runs.
\
\ Each MODEL: line runs at load time (a parse-time IR build, like the slice-1 device test);
\ LRD-GOLD1 lowers + launches whatever IR is current, so the same model text feeds the host
\ reference, the launch staging, and the spawned child that emits the kernel PTX. Not part of
\ the maki gate (maki/test.f) - it needs the CUDA toolkit + a device. Run on the Orin: scp to
\ zed:Work/habu then `bin/hb --load maki/lower-red-device-test.f`.

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

create LRD-OUT $4000 allot  create LRD-ERR $1000 allot
create LRD-QO  $1000 allot  create LRD-QE  $2000 allot

\ ---- spawn bin/hb to emit region 0's PTX into PTXTC:PTX$ (child re-builds the IR) --
: LRD-EMIT ( ptr u8 n -- ) {: sa:ptr su:n :}
   sa su  s" require maki/lower-red.f"  s" LRED-EMIT"  0 FP-REGION-ID  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   s" bin/hb" >LEN  LRD-OUT $4000 >LEN  LRD-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   LRD-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ surface stderr + throw on a nonzero child
   PTXTC:PTX$ LRD-OUT outu LEN>N WRITE-ALL ;

: LRD-PTXAS ( -- n )  LRD-QO $1000 >LEN LRD-QE $2000 >LEN PTXTC:ASSEMBLE ;

\ ---- per-element evidence (device value | host value rounded to f32) --------
: LRD-FP ( r -- )  SB-RESET 6 SB-FIX SB$ type ;
: LRD-HOST@ ( n -- r )  LLA-OUT-NODE@ EX-OUT@ swap T-GET  F64>F32 F32>F64 ;   \ narrowed host elem
: LRD-EVIDENCE ( -- )
   s" elem   device        host_f32" type cr
   LLA-ELEMS@ 0 ?do
      s"   " type i SB-RESET SB-INT SB$ type s"    " type
      i LLA-OUT@ LRD-FP  s"    " type  i LRD-HOST@ LRD-FP  cr
   loop ;

\ ---- one model: emit -> assemble -> golden -> evidence -> assert V-PASS ------
\ Skips (no emit/ptxas) off-device, so the file still loads where there is no CUDA.
: LRD-GOLD1 ( ptr u8 n -- ) {: sa:ptr su:n :}
   CUDA:OPEN? 0= if exit then
   sa su LRD-EMIT
   LRD-PTXAS PTXTC:ASM-REPORT 0 T=                        \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LLA-CUBIN!
   0 FP-REGION-ID LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   LRD-EVIDENCE
   v V-PASS T= ;

: LRD-BEGIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" lower-red-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      exit then
   s" habu-lred-drv" MAKI-GRADE:PREPARE
   s" habu-lred-ptx" PTXTC:PREPARE ;

: LRD-END ( -- )
   CUDA:OPEN? 0= if  0 0= TTRUE  T-REPORT exit then
   PTXTC:CLEAN  MAKI-GRADE:CLEAN
   T-REPORT ;

end-package

package MAKI
LRD-BEGIN

s" == RMSNORM 4x8 ==" type cr
MODEL: RM ( x:4x8 -- y ) RMSNORM ;  FP-BUILD
s" MODEL: RM ( x:4x8 -- y ) RMSNORM ;" LRD-GOLD1

s" == LAYERNORM 4x8 ==" type cr
MODEL: LN ( x:4x8 -- y ) LAYERNORM ;  FP-BUILD
s" MODEL: LN ( x:4x8 -- y ) LAYERNORM ;" LRD-GOLD1

s" == SOFTMAX-ROW 4x8 ==" type cr
MODEL: SM ( x:4x8 -- y ) SOFTMAX-ROW ;  FP-BUILD
s" MODEL: SM ( x:4x8 -- y ) SOFTMAX-ROW ;" LRD-GOLD1

s" == GELU->RMSNORM 4x8 ==" type cr
MODEL: GR ( x:4x8 -- y ) GELU RMSNORM ;  FP-BUILD
s" MODEL: GR ( x:4x8 -- y ) GELU RMSNORM ;" LRD-GOLD1

\ BIAS->RMSNORM 4x8 + 1x8: a 1xC row-broadcast prologue pinned to row 0 (same row/block);
\ the device kernel reads element tid of the single bias row and the host reads EX-BC@ 1xC.
s" == BIAS->RMSNORM 4x8 + 1x8 (1xC row-broadcast) ==" type cr
MODEL: BR ( x:4x8 b:1x8 -- y ) BIAS RMSNORM ;  FP-BUILD
s" MODEL: BR ( x:4x8 b:1x8 -- y ) BIAS RMSNORM ;" LRD-GOLD1

\ SCALE->RMSNORM 4x8 + 1x1: a 1x1 scalar-broadcast prologue (row 0 + zero column offset);
\ every lane reads element 0 on both legs.
s" == SCALE->RMSNORM 4x8 + 1x1 (1x1 scalar-broadcast) ==" type cr
MODEL: SR ( x:4x8 s:1x1 -- y ) SCALE RMSNORM ;  FP-BUILD
s" MODEL: SR ( x:4x8 s:1x1 -- y ) SCALE RMSNORM ;" LRD-GOLD1

LRD-END
end-package
