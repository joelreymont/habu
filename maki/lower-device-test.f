\ maki/lower-device-test.f - the Orin device leg for the elementwise region lowering.
\
\ CAD-PLAN section 11 device-vs-host GOLDEN, slice 1. Builds a GELU->RELU model,
\ fuses it, and lowers region 0 onto a REGION_0 PTX kernel: spawn a fresh bin/hb to
\ EMIT the kernel to PTXTC:PTX$, EMIT-GUARD, ptxas ASSEMBLE + ASM-REPORT, then
\ LOWER-GOLDEN (maki/lower-golden.f) runs the host executor and the device kernel on
\ the same synthetic inputs and compares under the f32 tolerance. Off the Orin
\ (no libcuda) the device leg is reported SKIPPED and the host build still runs.
\
\ Not part of the maki gate (maki/test.f) - it needs the CUDA toolkit + a device.
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

\ The model text appears twice, identically: the literal MODEL: line builds the IR
\ in THIS process (for the host reference + launch), and LD-SRC$ carries the same
\ text into the spawned child that emits the kernel PTX.
MODEL: GELU_RELU ( x:4x8 -- y ) GELU RELU ;
FP-BUILD
: LD-SRC$ ( -- ptr u8 n )  s" MODEL: GELU_RELU ( x:4x8 -- y ) GELU RELU ;" ;

create LD-OUT $4000 allot  create LD-ERR $1000 allot
create LD-QO  $1000 allot  create LD-QE  $2000 allot

\ ---- spawn bin/hb to emit region 0's PTX into PTXTC:PTX$ --------------------
: LD-EMIT ( -- )
   LD-SRC$ 0 MAKI-GRADE:DRIVER$ LEW-WRITE-DRIVER
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
: LD-HOST@ ( n -- r )  LEW-OUT-NODE@ EX-OUT@ swap T-GET  F64>F32 F32>F64 ;   \ narrowed host elem
: LD-EVIDENCE ( -- )
   s" elem   device        host_f32" type cr
   LEW-ELEMS 0 ?do
      s"   " type i SB-RESET SB-INT SB$ type s"    " type
      i LLA-OUT@ LD-FP  s"    " type  i LD-HOST@ LD-FP  cr
   loop ;

: LD-MAIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" lower-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      0 0= TTRUE
      T-REPORT exit
   then
   s" habu-lower-drv" MAKI-GRADE:PREPARE
   s" habu-lower-ptx" PTXTC:PREPARE
   LD-EMIT
   LD-PTXAS PTXTC:ASM-REPORT 0 T=                         \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LLA-CUBIN!
   0 LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   LD-EVIDENCE
   v V-PASS T=
   PTXTC:CLEAN  MAKI-GRADE:CLEAN
   T-REPORT ;

LD-MAIN

end-package
