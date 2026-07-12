\ maki/lower-device-test.f - the Orin device leg for the elementwise region lowering.
\
\ CAD-PLAN section 11 device-vs-host GOLDEN, slice 1. For each elementwise model it builds
\ the model, fuses it, and lowers region 0 onto a REGION_0 flat PTX kernel: spawn a fresh
\ bin/hb (via LOWER-DRIVER!) to EMIT the kernel to PTXTC:PTX$, EMIT-GUARD, ptxas ASSEMBLE +
\ ASM-REPORT, then LOWER-GOLDEN (maki/lower-golden.f) runs the host executor and the device
\ kernel on the same synthetic inputs and compares under the f32 tolerance. Cases: a GELU->RELU
\ chain, a BIAS 1xC row-broadcast (device loads [e mod C]), and a SCALE 1x1 scalar-broadcast
\ (device loads [0]) - the broadcast legs prove the load-index remap matches the host EX-BC@.
\ Off the Orin (no libcuda) the device leg is reported SKIPPED and the host build still runs.
\
\ Each MODEL: line runs at load time; LD-GOLD1 lowers + launches whatever IR is current, so the
\ same model text feeds the host reference, the launch staging, and the spawned child that emits
\ the kernel PTX. Not part of the maki gate (maki/test.f) - it needs the CUDA toolkit + a device.
\ Run on the Orin: scp to zed:Work/habu then `bin/hb --load maki/lower-device-test.f`.

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

create LD-OUT $4000 allot  create LD-ERR $1000 allot
create LD-QO  $1000 allot  create LD-QE  $2000 allot

\ ---- spawn bin/hb to emit region 0's PTX into PTXTC:PTX$ (child re-builds the IR) --
: LD-EMIT ( ptr u8 n -- ) {: sa:ptr su:n :}
   sa su  s" require maki/lower-ew.f"  s" LEW-EMIT"  0 FP-REGION-ID  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   s" bin/hb" >LEN  LD-OUT $4000 >LEN  LD-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   LD-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ surface stderr + throw on a nonzero child
   PTXTC:PTX$ LD-OUT outu LEN>N WRITE-ALL ;

: LD-PTXAS ( -- n )  LD-QO $1000 >LEN LD-QE $2000 >LEN PTXTC:ASSEMBLE ;

\ ---- per-element evidence (device value | host value rounded to f32) --------
: LD-FP ( r -- )  SB-RESET 6 SB-FIX SB$ type ;
: LD-HOST@ ( n -- r )  LLA-OUT-NODE@ EX-OUT@ swap T-GET  F64>F32 F32>F64 ;   \ narrowed host elem
: LD-EVIDENCE ( -- )
   s" elem   device        host_f32" type cr
   LLA-ELEMS@ 0 ?do
      s"   " type i SB-RESET SB-INT SB$ type s"    " type
      i LLA-OUT@ LD-FP  s"    " type  i LD-HOST@ LD-FP  cr
   loop ;

\ ---- one model: emit -> assemble -> golden -> evidence -> assert V-PASS ------
\ Skips (no emit/ptxas) off-device, so the file still loads where there is no CUDA.
: LD-GOLD1 ( ptr u8 n -- ) {: sa:ptr su:n :}
   CUDA:OPEN? 0= if exit then
   sa su LD-EMIT
   LD-PTXAS PTXTC:ASM-REPORT 0 T=                         \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LLA-CUBIN!
   0 FP-REGION-ID LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   LD-EVIDENCE
   v V-PASS T= ;

: LD-BEGIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" lower-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      exit then
   s" habu-lower-drv" MAKI-GRADE:PREPARE
   s" habu-lower-ptx" PTXTC:PREPARE ;

: LD-END ( -- )
   CUDA:OPEN? 0= if  0 0= TTRUE  T-REPORT exit then
   PTXTC:CLEAN  MAKI-GRADE:CLEAN
   T-REPORT ;

;package

package MAKI
LD-BEGIN

s" == GELU->RELU 4x8 ==" type cr
MODEL: GELU_RELU ( x:4x8 -- y ) GELU RELU ;  FP-BUILD
s" MODEL: GELU_RELU ( x:4x8 -- y ) GELU RELU ;" LD-GOLD1

\ BIAS 4x8 + 1x8: the bias operand is a 1xC row-broadcast; the device kernel loads [e mod C]
\ (rem.u32) and the host executor reads EX-BC@ 1xC, so the two outputs must match op-for-op.
s" == BIAS 4x8 + 1x8 (1xC row-broadcast) ==" type cr
MODEL: MB ( x:4x8 b:1x8 -- y ) BIAS ;  FP-BUILD
s" MODEL: MB ( x:4x8 b:1x8 -- y ) BIAS ;" LD-GOLD1

\ SCALE 4x8 + 1x1: the scale operand is a 1x1 scalar-broadcast; the device kernel loads [0]
\ (mov.u64 0) and the host executor reads EX-BC@ 1x1.
s" == SCALE 4x8 + 1x1 (1x1 scalar-broadcast) ==" type cr
MODEL: MS ( x:4x8 s:1x1 -- y ) SCALE ;  FP-BUILD
s" MODEL: MS ( x:4x8 s:1x1 -- y ) SCALE ;" LD-GOLD1

LD-END
;package
