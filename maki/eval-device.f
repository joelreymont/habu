\ maki/eval-device.f - the device-golden autograder: GRADE = certify AND run-correct.
\
\ Closes the "grading is certification, not correctness" gap. GRADE-CANDIDATE takes
\ an arbitrary SAXPY candidate source and, per candidate: EVAL:CHECK-PASSES? (the type/
\ stack judge) -> write a driver that defines the kernel + emits it, spawn bin/hb to
\ produce ITS OWN PTX (a fresh top-level emit, captured) -> ptxas (subprocess) -> run
\ on the Orin (x=2,y=0,a=3) -> compare the SAXPY golden a*x+y=6.0. Verdict: 2 GREEN
\ (certify + device-correct), 1 TYPED-WRONG (certifies but device output != golden -
\ a semantic bug the checker can't catch), 0 REJECTED (does not certify). DEVICE-SCORE
\ tallies GREEN so pass@k means well-typed AND device-correct. Fully checked Habu.
\ Load after the PTX tile vocab; this file owns its stdlib/process setup.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/ffi.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/tile.f
require maki/eval.f
require maki/cuda-driver.f
require maki/device-artifacts.f
require lib/ptx/sentinel.f

\ eval-device reopens package EVAL as the SAXPY device-golden module. CHECK-PASSES?
\ (maki/eval.f) is same-package now, so it resolves bare; only the MAKI-GRADE:/CUDA:/
\ PTXSENT: artifact + driver packages stay qualified. The ED-*/GP-*/GQ- launch state,
\ the EVN- verdict codes, and the GRADE- driver helpers are private; the GRADE-
\ entrypoints and DEVICE- pass@k tally are the public surface.
package EVAL

private

\ ---- device gate: run a SAXPY cubin and compare the task golden ----
create ED-PATH 64 allot  create ED-KN 32 allot
variable ED-DEV variable ED-CTX variable ED-MOD variable ED-FUNC
variable ED-DX variable ED-DY variable ED-AB variable ED-NV variable ED-RBUF

\ verdict codes for the without-checker arm (eval-internal; the CMP- ablation reads them)
0 constant EVN-EMIT-FAIL
1 constant EVN-PTXAS-FAIL
2 constant EVN-DEVICE-WRONG
3 constant EVN-GREEN

: ED-RUN ( ptr u8 n -- n ) {: pa pu :}          \ cubin path -> f32 result bits
   ED-RBUF 4 PTXSENT:FILL                        \ poison readback: a dropped copy-back fails closed
   CUDA:OPEN
   0 CUDA:CUINIT CUDA:RC0
   ED-DEV 0 >IDX CUDA:CUDEVICEGET CUDA:RC0
   ED-CTX ED-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   ED-CTX @ >CUDA-CTX CUDA:CUCTXSETCURRENT CUDA:RC0
   pa pu ED-PATH >CSTR
   ED-MOD ED-PATH CUDA:CUMODULELOAD CUDA:RC0
   s" SAXPY" ED-KN >CSTR
   ED-FUNC ED-MOD @ >CUDA-MOD ED-KN CUDA:CUMODULEGETFUNCTION CUDA:RC0
   ED-DX 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   ED-DY 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   ED-DX @ >CUDA-DEVPTR $40000000 4 >COUNT CUDA:CUMEMSETD32 CUDA:RC0     \ x = 2.0
   ED-DY @ >CUDA-DEVPTR 0 4 >COUNT CUDA:CUMEMSETD32 CUDA:RC0             \ y = 0
   $40400000 ED-AB !  4 ED-NV !                                      \ a = 3.0, n = 4
   ED-FUNC @ >CUDA-FN 256 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   ED-FUNC @ >CUDA-FN 24 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   ED-FUNC @ >CUDA-FN 0 >IDX  ED-DX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   ED-FUNC @ >CUDA-FN 8 >IDX  ED-DY 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   ED-FUNC @ >CUDA-FN 16 >IDX ED-AB 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   ED-FUNC @ >CUDA-FN 20 >IDX ED-NV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   ED-FUNC @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   ED-RBUF ED-DY @ >CUDA-DEVPTR 4 >LEN CUDA:CUMEMCPYDTOH CUDA:RC0
   ED-DX @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   ED-DY @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   ED-MOD @ >CUDA-MOD CUDA:CUMODULEUNLOAD CUDA:RC0
   ED-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRELEASE CUDA:RC0
   ED-RBUF @ $FFFFFFFF and PTXSENT:GUARD ;
: DEVICE-CORRECT? ( ptr u8 n -- bool )  ED-RUN $40C00000 = ;   \ golden a*x+y = 6.0

\ ---- write a driver that defines the candidate kernel K and emits it to stdout ----
: GRADE-WRITE-DRIVER ( ptr u8 n -- ) {: a u :}
   SB-RESET
   s" 256 %BLOCK" SB-APPEND  10 SB-APPEND-C
   s" : " SB-APPEND  a u SB-APPEND  s"  ;" SB-APPEND  10 SB-APPEND-C
   s" CG-RESET CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS 1 SPAN-REG 2 SPAN-REG 1 UNIFORM-REG K CG-RET CG-CLOSE"
      SB-APPEND  10 SB-APPEND-C
   s" bye" SB-APPEND  10 SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

: GRADE-WRITE-UNCHECKED-DRIVER ( ptr u8 n -- ) {: a:ptr u:n :}
   SB-RESET
   s" 0 set-check" SB-APPEND  10 SB-APPEND-C
   s" 256 %BLOCK" SB-APPEND  10 SB-APPEND-C
   s" : " SB-APPEND  a u SB-APPEND  s"  ;" SB-APPEND  10 SB-APPEND-C
   s" CG-RESET CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS 1 SPAN-REG 2 SPAN-REG 1 UNIFORM-REG K CG-RET CG-CLOSE"
      SB-APPEND  10 SB-APPEND-C
   s" bye" SB-APPEND  10 SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

\ ---- spawn bin/hb to emit the driver's PTX (captured stdout) ----
\ A child process, not in-process compilation, because the driver embeds the
\ CANDIDATE kernel under eval - untrusted generated source. In-process it would
\ mutate this grader's dictionary, and a crashing or hanging candidate would
\ kill the grader; the child gives rc + captured stderr + a timeout, so a bad
\ candidate is a graded failure, never a grader casualty.
create GP-OUT $4000 allot  create GP-ERR $1000 allot
: GRADE-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/tile.f" >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$       >LEN PROC-ARGV+
   s" bin/hb" >LEN  GP-OUT $4000 >LEN  GP-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   \ the emit process exits with the FFI-file convention code, not 0; the real
   \ signal is the captured PTX on stdout. Write it regardless; ptxas validates it.
   MAKI-GRADE:PTX$ GP-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

\ ---- assemble PTX to cubin via ptxas; return rc ----
create GQ-OUT $1000 allot  create GQ-ERR $1000 allot
: GRADE-PTXAS ( -- n )
   PROC-ARGV-RESET
   s" -arch=sm_87"          >LEN PROC-ARGV+
   MAKI-GRADE:PTX$          >LEN PROC-ARGV+
   s" -o"                   >LEN PROC-ARGV+
   MAKI-GRADE:CUBIN$        >LEN PROC-ARGV+
   MAKI-GRADE:PTXAS$        >LEN  GQ-OUT $1000 >LEN  GQ-ERR $1000 >LEN  10000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}  rc RC>N ;

: GRADE-DEVICE-VERDICT ( -- n )
   MAKI-GRADE:CUBIN$ DEVICE-CORRECT? if 2 else 1 then ;

public

\ ---- the general grade: 2 GREEN / 1 TYPED-BUT-WRONG / 0 REJECTED ----
: GRADE-CANDIDATE ( ptr u8 n -- n ) {: a u :}
   a u CHECK-PASSES? 0= if 0 exit then                 \ checker rejects -> 0
   s" habu-grade-saxpy" MAKI-GRADE:PREPARE
   a u GRADE-WRITE-DRIVER
   GRADE-EMIT  0 = if MAKI-GRADE:CLEAN 1 exit then
   GRADE-PTXAS 0 <> if MAKI-GRADE:CLEAN 1 exit then
   GRADE-DEVICE-VERDICT {: v:n :}
   MAKI-GRADE:CLEAN
   v ;

: GRADE-NOCHECK-CANDIDATE ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" habu-grade-saxpy" MAKI-GRADE:PREPARE
   a u GRADE-WRITE-UNCHECKED-DRIVER
   GRADE-EMIT 0 = if MAKI-GRADE:CLEAN EVN-EMIT-FAIL exit then
   GRADE-PTXAS 0 <> if MAKI-GRADE:CLEAN EVN-PTXAS-FAIL exit then
   GRADE-DEVICE-VERDICT 2 = if EVN-GREEN else EVN-DEVICE-WRONG then {: v:n :}
   MAKI-GRADE:CLEAN
   v ;

\ pass@k that means certify AND device-correct
variable DEVICE-PASS  variable DEVICE-TOTAL
: DEVICE-RESET ( -- )  0 DEVICE-PASS !  0 DEVICE-TOTAL ! ;
: DEVICE-SCORE ( ptr u8 n -- )
   GRADE-CANDIDATE  2 = if DEVICE-PASS @ 1+ DEVICE-PASS ! then  DEVICE-TOTAL @ 1+ DEVICE-TOTAL ! ;

end-package
