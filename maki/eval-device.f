\ maki/eval-device.f - the device-golden autograder: GRADE = certify AND run-correct.
\
\ Closes the "grading is certification, not correctness" gap. GRADE-CANDIDATE takes
\ an arbitrary SAXPY candidate source and, per candidate: CHECK-PASSES? (the type/
\ stack judge) -> write a driver that defines the kernel + emits it, spawn bin/hb to
\ produce ITS OWN PTX (a fresh top-level emit, captured) -> ptxas (subprocess) -> run
\ on the Orin (x=2,y=0,a=3) -> compare the SAXPY golden a*x+y=6.0. Verdict: 2 GREEN
\ (certify + device-correct), 1 TYPED-WRONG (certifies but device output != golden -
\ a semantic bug the checker can't catch), 0 REJECTED (does not certify). EVD-SCORE
\ tallies GREEN so pass@k means well-typed AND device-correct. Fully checked Habu.
\ Load after the PTX tile vocab, maki/eval.f, lib/ffi.f, and the fs/process libs:
\   lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f.

\ ---- device gate: run a SAXPY cubin and compare the task golden ----
create ED-LIB 16 allot  create ED-NM 64 allot  create ED-PATH 64 allot  create ED-KN 32 allot
variable ED-H variable ED-DEV variable ED-CTX variable ED-MOD variable ED-FUNC
variable ED-DX variable ED-DY variable ED-AB variable ED-NV variable ED-RBUF
: ED-SYM ( ptr u8 n -- n )  ED-NM >CSTR  ED-H @ ED-NM DLSYM ;
: ED-RUN ( ptr u8 n -- n ) {: pa pu :}          \ cubin path -> f32 result bits
   s" libcuda.so.1" ED-LIB >CSTR  ED-LIB RTLD-NOW DLOPEN ED-H !
   0                       s" cuInit"                   ED-SYM CALL1 drop
   ED-DEV P>N 0            s" cuDeviceGet"              ED-SYM CALL2 drop
   ED-CTX P>N ED-DEV @     s" cuDevicePrimaryCtxRetain" ED-SYM CALL2 drop
   ED-CTX @               s" cuCtxSetCurrent"          ED-SYM CALL1 drop
   pa pu ED-PATH >CSTR
   ED-MOD P>N ED-PATH P>N s" cuModuleLoad"             ED-SYM CALL2 drop
   s" SAXPY" ED-KN >CSTR
   ED-FUNC P>N ED-MOD @ ED-KN P>N s" cuModuleGetFunction" ED-SYM CALL3 drop
   ED-DX P>N 16           s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   ED-DY P>N 16           s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   ED-DX @ $40000000 4    s" cuMemsetD32_v2"  ED-SYM CALL3 drop      \ x = 2.0
   ED-DY @ 0 4            s" cuMemsetD32_v2"  ED-SYM CALL3 drop      \ y = 0
   $40400000 ED-AB !  4 ED-NV !                                      \ a = 3.0, n = 4
   ED-FUNC @ 256 1 1      s" cuFuncSetBlockShape" ED-SYM CALL4 drop
   ED-FUNC @ 24           s" cuParamSetSize"  ED-SYM CALL2 drop
   ED-FUNC @ 0  ED-DX P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 8  ED-DY P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 16 ED-AB P>N 4  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 20 ED-NV P>N 4  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 1 1          s" cuLaunchGrid"    ED-SYM CALL3 drop
   0                      s" cuCtxSynchronize" ED-SYM CALL1 drop
   ED-RBUF P>N ED-DY @ 4  s" cuMemcpyDtoH_v2" ED-SYM CALL3 drop
   ED-MOD @  s" cuModuleUnload"            ED-SYM CALL1 drop
   ED-DEV @  s" cuDevicePrimaryCtxRelease" ED-SYM CALL1 drop
   ED-RBUF @ $FFFFFFFF and ;
: DEVICE-CORRECT? ( ptr u8 n -- bool )  ED-RUN $40C00000 = ;   \ golden a*x+y = 6.0

\ ---- write a driver that defines the candidate kernel K and emits it to stdout ----
: GRADE-WRITE-DRIVER ( ptr u8 n -- ) {: a u :}
   SB-RESET
   s" 256 %BLOCK" SB-APPEND  10 SB-APPEND-C
   s" : " SB-APPEND  a u SB-APPEND  s"  ;" SB-APPEND  10 SB-APPEND-C
   s" CG-RESET CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS 1 SPAN-REG 2 SPAN-REG 1 UNIFORM-REG K CG-RET CG-CLOSE"
      SB-APPEND  10 SB-APPEND-C
   s" bye" SB-APPEND  10 SB-APPEND-C
   s" /tmp/grade-driver.f" SB$ WRITE-ALL ;

: GRADE-WRITE-UNCHECKED-DRIVER ( ptr u8 n -- ) {: a:ptr u:n :}
   SB-RESET
   s" 0 set-check" SB-APPEND  10 SB-APPEND-C
   s" 256 %BLOCK" SB-APPEND  10 SB-APPEND-C
   s" : " SB-APPEND  a u SB-APPEND  s"  ;" SB-APPEND  10 SB-APPEND-C
   s" CG-RESET CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS 1 SPAN-REG 2 SPAN-REG 1 UNIFORM-REG K CG-RET CG-CLOSE"
      SB-APPEND  10 SB-APPEND-C
   s" bye" SB-APPEND  10 SB-APPEND-C
   s" /tmp/grade-driver.f" SB$ WRITE-ALL ;

\ ---- spawn bin/hb to emit the driver's PTX (captured stdout) -> /tmp/grade.ptx ----
create GP-OUT $4000 allot  create GP-ERR $1000 allot
: GRADE-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/tile.f" >LEN PROC-ARGV+
   s" /tmp/grade-driver.f"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  GP-OUT $4000 >LEN  GP-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   \ the emit process exits with the FFI-file convention code, not 0; the real
   \ signal is the captured PTX on stdout. Write it regardless; ptxas validates it.
   s" /tmp/grade.ptx" GP-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

\ ---- assemble /tmp/grade.ptx -> /tmp/grade.cubin via ptxas; return rc ----
create GQ-OUT $1000 allot  create GQ-ERR $1000 allot
: GRADE-PTXAS ( -- n )
   PROC-ARGV-RESET
   s" -arch=sm_87"          >LEN PROC-ARGV+
   s" /tmp/grade.ptx"       >LEN PROC-ARGV+
   s" -o"                   >LEN PROC-ARGV+
   s" /tmp/grade.cubin"     >LEN PROC-ARGV+
   s" /usr/local/cuda-12.6/bin/ptxas" >LEN  GQ-OUT $1000 >LEN  GQ-ERR $1000 >LEN  10000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}  rc RC>N ;

\ ---- the general grade: 2 GREEN / 1 TYPED-BUT-WRONG / 0 REJECTED ----
: GRADE-CANDIDATE ( ptr u8 n -- n ) {: a u :}
   a u CHECK-PASSES? 0= if 0 exit then            \ checker rejects -> 0
   a u GRADE-WRITE-DRIVER
   GRADE-EMIT  0 = if 1 exit then                 \ no PTX produced
   GRADE-PTXAS 0 <> if 1 exit then                \ won't assemble
   s" /tmp/grade.cubin" DEVICE-CORRECT? if 2 else 1 then ;

0 constant EVN-EMIT-FAIL
1 constant EVN-PTXAS-FAIL
2 constant EVN-DEVICE-WRONG
3 constant EVN-GREEN

: GRADE-NOCHECK-CANDIDATE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u GRADE-WRITE-UNCHECKED-DRIVER
   GRADE-EMIT 0 = if EVN-EMIT-FAIL exit then
   GRADE-PTXAS 0 <> if EVN-PTXAS-FAIL exit then
   s" /tmp/grade.cubin" DEVICE-CORRECT? if EVN-GREEN else EVN-DEVICE-WRONG then ;

\ pass@k that means certify AND device-correct
variable EVD-PASS  variable EVD-TOTAL
: EVD-RESET ( -- )  0 EVD-PASS !  0 EVD-TOTAL ! ;
: EVD-SCORE ( ptr u8 n -- )
   GRADE-CANDIDATE  2 = if EVD-PASS @ 1+ EVD-PASS ! then  EVD-TOTAL @ 1+ EVD-TOTAL ! ;
