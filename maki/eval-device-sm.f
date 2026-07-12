\ maki/eval-device-sm.f - the device-golden autograder for a SECOND task: softmax-rows.
\
\ Proves the GRADE = certify AND run-correct mechanism (maki/eval-device.f) is
\ task-general, not SAXPY-specific. The softmax authoring task uses block reductions
\ (ROW/BLOCK-MAX/B-/EXP./BLOCK-SUM/B/), and its sharpest authoring error is B- vs B/
\ (subtract instead of divide) - TYPE-IDENTICAL, so it certifies, but the device gate
\ catches it. GRADE-SM: EVAL:CHECK-PASSES? -> spawn bin/hb to emit the candidate's softmax
\ PTX -> ptxas -> run on the Orin (row [1,2,3,4]) -> compare softmax([1,2,3,4]) within
\ tolerance. Verdict 2 GREEN / 1 TYPED-WRONG / 0 REJECTED. Load after the PTX tile +
\ collective vocab and `lib/ptx/launch.f`; this file owns stdlib/process setup.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/ffi.f
require maki/eval.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f
require lib/ptx/launch.f
require lib/ptx/collective.f
require maki/cuda-driver.f
require maki/device-artifacts.f
require lib/ptx/sentinel.f

\ eval-device-sm reopens package EVAL as the softmax-rows device-golden module,
\ mirroring maki/eval-device.f. CHECK-PASSES? is same-package (bare); the SM- launch
\ state and GRADE-SM- driver helpers are private, GRADE-SM the one public entrypoint.
package EVAL

private

create SM-PATH 64 allot  create SM-KN 32 allot
create SM-IN 16 allot   create SM-OUT 16 allot   create SMG 4 cells allot
variable SM-DEV variable SM-CTX variable SM-MOD variable SM-FUNC
variable SM-DI variable SM-DO variable SM-KV
: SM-F32! ( n ptr u8 n -- ) {: v buf idx :} idx 4 * {: o :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;
: SM-F32@ ( n -- n ) {: idx :} idx 4 * {: o :}
   SM-OUT o + c@  SM-OUT o 1 + + c@ 8 lshift or  SM-OUT o 2 + + c@ 16 lshift or  SM-OUT o 3 + + c@ 24 lshift or ;
: SM-INIT ( -- )                                   \ input [1,2,3,4] + golden softmax bits
   1.0 F64>F32 SM-IN 0 SM-F32!  2.0 F64>F32 SM-IN 1 SM-F32!
   3.0 F64>F32 SM-IN 2 SM-F32!  4.0 F64>F32 SM-IN 3 SM-F32!
   1023627234 SMG 0 cells + !  1035106489 SMG 1 cells + !
   1047695721 SMG 2 cells + !  1059379089 SMG 3 cells + ! ;

: SM-RUN ( ptr u8 n -- ) {: pa pu :}               \ run softmax cubin, fill SM-OUT
   SM-OUT 16 PTXSENT:FILL                          \ poison readback: a dropped copy-back fails closed
   1 4 256 PTX-ROW-LAUNCH-CHECK
   CUDA:OPEN
   0 CUDA:CUINIT CUDA:RC0
   SM-DEV 0 >IDX CUDA:CUDEVICEGET CUDA:RC0
   SM-CTX SM-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   SM-CTX @ >CUDA-CTX CUDA:CUCTXSETCURRENT CUDA:RC0
   pa pu SM-PATH >CSTR
   SM-MOD SM-PATH CUDA:CUMODULELOAD CUDA:RC0
   s" SOFTMAX_ROWS" SM-KN >CSTR
   SM-FUNC SM-MOD @ >CUDA-MOD SM-KN CUDA:CUMODULEGETFUNCTION CUDA:RC0
   SM-DI 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   SM-DO 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   SM-DI @ >CUDA-DEVPTR SM-IN 16 >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   4 SM-KV !
   SM-FUNC @ >CUDA-FN 256 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   SM-FUNC @ >CUDA-FN 20 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   SM-FUNC @ >CUDA-FN 0 >IDX  SM-DI 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   SM-FUNC @ >CUDA-FN 8 >IDX  SM-DO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   SM-FUNC @ >CUDA-FN 16 >IDX SM-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   SM-FUNC @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   SM-OUT SM-DO @ >CUDA-DEVPTR 16 >LEN CUDA:CUMEMCPYDTOH CUDA:RC0
   SM-DI @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   SM-DO @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   SM-MOD @ >CUDA-MOD CUDA:CUMODULEUNLOAD CUDA:RC0
   SM-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRELEASE CUDA:RC0 ;

: SM-NEAR? ( n n -- bool )  - abs 8 <= ;            \ within 8 ULP (ex2.approx)
: DEVICE-CORRECT-SM? ( ptr u8 n -- bool )
   SM-RUN
   0 SM-F32@ PTXSENT:GUARD SMG 0 cells + @ SM-NEAR?
   1 SM-F32@ PTXSENT:GUARD SMG 1 cells + @ SM-NEAR? and
   2 SM-F32@ PTXSENT:GUARD SMG 2 cells + @ SM-NEAR? and
   3 SM-F32@ PTXSENT:GUARD SMG 3 cells + @ SM-NEAR? and ;

\ ---- write a softmax driver that defines K and emits it (CG-SM-* scaffolding) ----
: GRADE-SM-WRITE-DRIVER ( ptr u8 n -- ) {: a u :}
   SB-RESET
   s" 256 %BLOCK" SB-APPEND  10 SB-APPEND-C
   s" : " SB-APPEND  a u SB-APPEND  s"  ;" SB-APPEND  10 SB-APPEND-C
   s" CG-SM-RESET CG-HEADER CG-SM-ENTRY CG-SM-OPEN CG-SM-PARAMS 1 MATRIX-REG 2 MATRIX-REG K CG-SM-RET CG-SM-CLOSE"
      SB-APPEND  10 SB-APPEND-C
   s" bye" SB-APPEND  10 SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

create GSP-OUT $8000 allot  create GSP-ERR $1000 allot
: GRADE-SM-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"        >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"           >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"        >LEN PROC-ARGV+
   s" lib/ptx/header.f" >LEN PROC-ARGV+  s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/collective.f" >LEN PROC-ARGV+  MAKI-GRADE:DRIVER$ >LEN PROC-ARGV+
   s" bin/hb" >LEN  GSP-OUT $8000 >LEN  GSP-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   MAKI-GRADE:PTX$ GSP-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

create GSQ-OUT $1000 allot  create GSQ-ERR $1000 allot
: GRADE-SM-PTXAS ( -- n )
   PROC-ARGV-RESET
   s" -arch=sm_87"      >LEN PROC-ARGV+
   MAKI-GRADE:PTX$      >LEN PROC-ARGV+
   s" -o"               >LEN PROC-ARGV+
   MAKI-GRADE:CUBIN$    >LEN PROC-ARGV+
   MAKI-GRADE:PTXAS$    >LEN  GSQ-OUT $1000 >LEN  GSQ-ERR $1000 >LEN  10000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}  rc RC>N ;

: GRADE-SM-DEVICE-VERDICT ( -- n )
   MAKI-GRADE:CUBIN$ DEVICE-CORRECT-SM? if 2 else 1 then ;

public

: GRADE-SM ( ptr u8 n -- n ) {: a u :}
   a u CHECK-PASSES? 0= if 0 exit then
   s" habu-grade-softmax" MAKI-GRADE:PREPARE
   a u GRADE-SM-WRITE-DRIVER
   GRADE-SM-EMIT  0 = if MAKI-GRADE:CLEAN 1 exit then
   GRADE-SM-PTXAS 0 <> if MAKI-GRADE:CLEAN 1 exit then
   GRADE-SM-DEVICE-VERDICT {: v:n :}
   MAKI-GRADE:CLEAN
   v ;

SM-INIT

;package

\ This file is now the GRADE-SM LIBRARY (mirrors maki/eval-device.f). The device-golden
\ test candidates moved to maki/eval-device-sm-test.f so the grader can be reused by
\ maki/eval-author.f without a trailing `bye`.
