\ maki/eval-device-sm.f - the device-golden autograder for a SECOND task: softmax-rows.
\
\ Proves the GRADE = certify AND run-correct mechanism (maki/eval-device.f) is
\ task-general, not SAXPY-specific. The softmax authoring task uses block reductions
\ (ROW/BLOCK-MAX/B-/EXP./BLOCK-SUM/B/), and its sharpest authoring error is B- vs B/
\ (subtract instead of divide) - TYPE-IDENTICAL, so it certifies, but the device gate
\ catches it. GRADE-SM: CHECK-PASSES? -> spawn bin/hb to emit the candidate's softmax
\ PTX -> ptxas -> run on the Orin (row [1,2,3,4]) -> compare softmax([1,2,3,4]) within
\ tolerance. Verdict 2 GREEN / 1 TYPED-WRONG / 0 REJECTED. Load after the PTX tile +
\ collective vocab, lib/ptx/launch.f, maki/eval.f, lib/ffi.f, and the
\ fs/process libs.

create SM-LIB 16 allot  create SM-NM 64 allot  create SM-PATH 64 allot  create SM-KN 32 allot
create SM-IN 16 allot   create SM-OUT 16 allot   create SMG 4 cells allot
variable SM-H variable SM-DEV variable SM-CTX variable SM-MOD variable SM-FUNC
variable SM-DI variable SM-DO variable SM-KV
: SM-F32! ( n ptr u8 n -- ) {: v buf idx :} idx 4 * {: o :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;
: SM-F32@ ( n -- n ) {: idx :} idx 4 * {: o :}
   SM-OUT o + c@  SM-OUT o 1 + + c@ 8 lshift or  SM-OUT o 2 + + c@ 16 lshift or  SM-OUT o 3 + + c@ 24 lshift or ;
: SM-SYM ( ptr u8 n -- n )  SM-NM >CSTR  SM-H @ SM-NM DLSYM ;

: SM-INIT ( -- )                                   \ input [1,2,3,4] + golden softmax bits
   1.0 F64>F32 SM-IN 0 SM-F32!  2.0 F64>F32 SM-IN 1 SM-F32!
   3.0 F64>F32 SM-IN 2 SM-F32!  4.0 F64>F32 SM-IN 3 SM-F32!
   1023627234 SMG 0 cells + !  1035106489 SMG 1 cells + !
   1047695721 SMG 2 cells + !  1059379089 SMG 3 cells + ! ;

: SM-RUN ( ptr u8 n -- ) {: pa pu :}               \ run softmax cubin, fill SM-OUT
   1 4 256 PTX-ROW-LAUNCH-CHECK
   s" libcuda.so.1" SM-LIB >CSTR  SM-LIB RTLD-NOW DLOPEN SM-H !
   0                       s" cuInit"                   SM-SYM CALL1 drop
   SM-DEV P>N 0            s" cuDeviceGet"              SM-SYM CALL2 drop
   SM-CTX P>N SM-DEV @     s" cuDevicePrimaryCtxRetain" SM-SYM CALL2 drop
   SM-CTX @               s" cuCtxSetCurrent"          SM-SYM CALL1 drop
   pa pu SM-PATH >CSTR
   SM-MOD P>N SM-PATH P>N s" cuModuleLoad"             SM-SYM CALL2 drop
   s" SOFTMAX_ROWS" SM-KN >CSTR
   SM-FUNC P>N SM-MOD @ SM-KN P>N s" cuModuleGetFunction" SM-SYM CALL3 drop
   SM-DI P>N 16           s" cuMemAlloc_v2"   SM-SYM CALL2 drop
   SM-DO P>N 16           s" cuMemAlloc_v2"   SM-SYM CALL2 drop
   SM-DI @ SM-IN P>N 16   s" cuMemcpyHtoD_v2" SM-SYM CALL3 drop
   4 SM-KV !
   SM-FUNC @ 256 1 1      s" cuFuncSetBlockShape" SM-SYM CALL4 drop
   SM-FUNC @ 20           s" cuParamSetSize"  SM-SYM CALL2 drop
   SM-FUNC @ 0  SM-DI P>N 8  s" cuParamSetv"  SM-SYM CALL4 drop
   SM-FUNC @ 8  SM-DO P>N 8  s" cuParamSetv"  SM-SYM CALL4 drop
   SM-FUNC @ 16 SM-KV P>N 4  s" cuParamSetv"  SM-SYM CALL4 drop
   SM-FUNC @ 1 1          s" cuLaunchGrid"    SM-SYM CALL3 drop
   0                      s" cuCtxSynchronize" SM-SYM CALL1 drop
   SM-OUT P>N SM-DO @ 16  s" cuMemcpyDtoH_v2" SM-SYM CALL3 drop
   SM-MOD @  s" cuModuleUnload"            SM-SYM CALL1 drop
   SM-DEV @  s" cuDevicePrimaryCtxRelease" SM-SYM CALL1 drop ;

: SM-NEAR? ( n n -- bool )  - abs 8 <= ;            \ within 8 ULP (ex2.approx)
: DEVICE-CORRECT-SM? ( ptr u8 n -- bool )
   SM-RUN
   0 SM-F32@ SMG 0 cells + @ SM-NEAR?
   1 SM-F32@ SMG 1 cells + @ SM-NEAR? and
   2 SM-F32@ SMG 2 cells + @ SM-NEAR? and
   3 SM-F32@ SMG 3 cells + @ SM-NEAR? and ;

\ ---- write a softmax driver that defines K and emits it (CG-SM-* scaffolding) ----
: GRADE-SM-WRITE-DRIVER ( ptr u8 n -- ) {: a u :}
   SB-RESET
   s" 256 %BLOCK" SB-APPEND  10 SB-APPEND-C
   s" : " SB-APPEND  a u SB-APPEND  s"  ;" SB-APPEND  10 SB-APPEND-C
   s" CG-SM-RESET CG-HEADER CG-SM-ENTRY CG-SM-OPEN CG-SM-PARAMS 1 MATRIX-REG 2 MATRIX-REG K CG-SM-RET CG-SM-CLOSE"
      SB-APPEND  10 SB-APPEND-C
   s" bye" SB-APPEND  10 SB-APPEND-C
   s" /tmp/grade-sm-driver.f" SB$ WRITE-ALL ;

create GSP-OUT $8000 allot  create GSP-ERR $1000 allot
: GRADE-SM-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"        >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"           >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"        >LEN PROC-ARGV+
   s" lib/ptx/header.f" >LEN PROC-ARGV+  s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/collective.f" >LEN PROC-ARGV+  s" /tmp/grade-sm-driver.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  GSP-OUT $8000 >LEN  GSP-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   s" /tmp/grade-sm.ptx" GSP-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

create GSQ-OUT $1000 allot  create GSQ-ERR $1000 allot
: GRADE-SM-PTXAS ( -- n )
   PROC-ARGV-RESET
   s" -arch=sm_87"      >LEN PROC-ARGV+
   s" /tmp/grade-sm.ptx"   >LEN PROC-ARGV+
   s" -o"               >LEN PROC-ARGV+
   s" /tmp/grade-sm.cubin"  >LEN PROC-ARGV+
   s" /usr/local/cuda-12.6/bin/ptxas" >LEN  GSQ-OUT $1000 >LEN  GSQ-ERR $1000 >LEN  10000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}  rc RC>N ;

: GRADE-SM ( ptr u8 n -- n ) {: a u :}
   a u CHECK-PASSES? 0= if 0 exit then
   a u GRADE-SM-WRITE-DRIVER
   GRADE-SM-EMIT  0 = if 1 exit then
   GRADE-SM-PTXAS 0 <> if 1 exit then
   s" /tmp/grade-sm.cubin" DEVICE-CORRECT-SM? if 2 else 1 then ;

SM-INIT

\ This file is now the GRADE-SM LIBRARY (mirrors maki/eval-device.f). The device-golden
\ test candidates moved to maki/eval-device-sm-test.f so the grader can be reused by
\ maki/eval-author.f without a trailing `bye`.
