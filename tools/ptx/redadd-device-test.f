\ redadd-device-test.f - device verification of red.global.add.f32 on sm_87 (closes
\ habu-ptx-ad-verify): the atomic float add scatter-add needs for fan-in adjoints. Spawns
\ bin/hb to emit tools/ptx/redadd-cg.f to a private PTX, ptxas-assembles, then launches 256
\ threads each atomically adding 1.0 to out[0] and asserts out[0] = 256.0 (FP32 exact). Orin-only.
\ Releases the primary context before exit (or bin/hb hangs at teardown). Load after lib/test.f,
\ lib/ffi.f, and the fs/process libs.

require lib/ptx/toolchain.f

create RA-LIB 16 allot  create RA-NM 64 allot  create RA-PATH 64 allot  create RA-KN 32 allot
variable RA-H variable RA-DEV variable RA-CTX variable RA-MOD variable RA-FUNC
variable RA-OUT variable RA-NV variable RA-RBUF
create RA-O $4000 allot  create RA-E $1000 allot  create RA-QO $1000 allot create RA-QE $1000 allot

: RA-SYM ( ptr u8 n -- n )  RA-NM >CSTR  RA-H @ RA-NM DLSYM ;

: RA-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+  s" tools/ptx/redadd-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  RA-O $4000 >LEN  RA-E $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: ou:len eu:len rc:rc :}  PTXTC:PTX$ RA-O ou LEN>N WRITE-ALL  ou LEN>N ;

: RA-PTXAS ( -- n )
   RA-QO $1000 >LEN RA-QE $1000 >LEN PTXTC:ASSEMBLE ;

: RA-RUN ( -- n )   \ launch 256 threads, out=0 -> out[0] f32 bits (= 256.0)
   s" libcuda.so.1" RA-LIB >CSTR  RA-LIB RTLD-NOW DLOPEN RA-H !
   0                      s" cuInit"                    RA-SYM CALL1 drop
   RA-DEV P>N 0           s" cuDeviceGet"               RA-SYM CALL2 drop
   RA-CTX P>N RA-DEV @    s" cuDevicePrimaryCtxRetain"  RA-SYM CALL2 drop
   RA-CTX @              s" cuCtxSetCurrent"           RA-SYM CALL1 drop
   PTXTC:CUBIN$ RA-PATH >CSTR
   RA-MOD P>N RA-PATH P>N s" cuModuleLoad"              RA-SYM CALL2 drop
   s" REDADD" RA-KN >CSTR
   RA-FUNC P>N RA-MOD @ RA-KN P>N s" cuModuleGetFunction" RA-SYM CALL3 drop
   RA-OUT P>N 16          s" cuMemAlloc_v2"   RA-SYM CALL2 drop
   RA-OUT @ 0 1           s" cuMemsetD32_v2"  RA-SYM CALL3 drop      \ out = 0
   256 RA-NV !
   RA-FUNC @ 256 1 1      s" cuFuncSetBlockShape" RA-SYM CALL4 drop
   RA-FUNC @ 12           s" cuParamSetSize"  RA-SYM CALL2 drop
   RA-FUNC @ 0  RA-OUT P>N 8  s" cuParamSetv"  RA-SYM CALL4 drop
   RA-FUNC @ 8  RA-NV P>N 4   s" cuParamSetv"  RA-SYM CALL4 drop
   RA-FUNC @ 1 1          s" cuLaunchGrid"    RA-SYM CALL3 drop
   0                      s" cuCtxSynchronize" RA-SYM CALL1 drop
   RA-RBUF P>N RA-OUT @ 4 s" cuMemcpyDtoH_v2" RA-SYM CALL3 drop
   RA-MOD @  s" cuModuleUnload"            RA-SYM CALL1 drop
   RA-DEV @  s" cuDevicePrimaryCtxRelease" RA-SYM CALL1 drop      \ release or bin/hb hangs at exit
   RA-RBUF @ $FFFFFFFF and ;

: REDADD-MAIN ( -- )
   T-RESET
   s" habu-ptx-redadd" PTXTC:PREPARE
   RA-EMIT drop
   RA-PTXAS 0 T=
   RA-RUN $43800000 T=                  \ 256 atomic adds of 1.0 = 256.0
   PTXTC:CLEAN
   s" device: red.global.add.f32 verified on sm_87 - 256 atomic adds = 256.0 (scatter-add primitive OK)" type cr
   T-REPORT ;

REDADD-MAIN
