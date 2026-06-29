\ softmax-launch.f - CHECKED on-device proof: launch the EMITTED SOFTMAX-ROWS
\ kernel on the Orin and verify it against the CPU golden.
\
\ Fully checked Habu via lib/ffi.f (only P>N/N>P trusted). The cubin is produced
\ by the checked kernel itself (tools/ptx/softmax-cg.f + ptxas -arch=sm_87), so
\ this closes the loop: the same body the ptx-stdlib gate certifies emits PTX
\ that runs numerically correct on hardware. Load after lib/errors.f, lib/string.f,
\ lib/test.f, lib/float.f, lib/fmt.f, src/arch/ptx/emit.f, lib/ptx/cg.f,
\ lib/ptx/header.f, lib/ptx/launch.f, lib/ffi.f.
\ Prereq: cubin at /tmp/softmax.cubin.
\
\ Data: in = [[1,2,3,4],[1,1,1,1]] (2 rows, k=4). Golden softmax (f32 bits):
\   row0 = 1023627234 1035106489 1047695721 1059379089   (~1 ULP, ex2.approx)
\   row1 = 1048576000 x4 = 0x3E800000 = 0.25 exactly.

create SL-LIB 16 allot  create SL-NM 64 allot  create SL-PATH 64 allot  create SL-KN 32 allot
create SL-HIN 32 allot  create SL-HOUT 32 allot
variable SL-H variable SL-DEV variable SL-CTX variable SL-MOD variable SL-FUNC
variable SL-DIN variable SL-DOUT variable SL-KV

: SL-F32! ( n ptr u8 n -- ) {: v buf idx :}
   idx 4 * {: o :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;
: SL-F32@ ( n -- n ) {: idx :}
   idx 4 * {: o :}
   SL-HOUT o + c@  SL-HOUT o 1 + + c@ 8 lshift or
   SL-HOUT o 2 + + c@ 16 lshift or  SL-HOUT o 3 + + c@ 24 lshift or ;

: SL-OPEN ( -- )  s" libcuda.so.1" SL-LIB >CSTR  SL-LIB RTLD-NOW DLOPEN SL-H ! ;
: SL-SYM ( ptr u8 n -- n )  SL-NM >CSTR  SL-H @ SL-NM DLSYM ;

: SL-PUT ( -- )                                       \ in = [1,2,3,4, 1,1,1,1]
   1.0 F64>F32 SL-HIN 0 SL-F32!  2.0 F64>F32 SL-HIN 1 SL-F32!
   3.0 F64>F32 SL-HIN 2 SL-F32!  4.0 F64>F32 SL-HIN 3 SL-F32!
   1.0 F64>F32 SL-HIN 4 SL-F32!  1.0 F64>F32 SL-HIN 5 SL-F32!
   1.0 F64>F32 SL-HIN 6 SL-F32!  1.0 F64>F32 SL-HIN 7 SL-F32! ;

: SL-SETUP ( -- )
   SL-OPEN
   0                       s" cuInit"                   SL-SYM CALL1 drop
   SL-DEV P>N 0            s" cuDeviceGet"              SL-SYM CALL2 drop
   SL-CTX P>N SL-DEV @     s" cuDevicePrimaryCtxRetain" SL-SYM CALL2 drop
   SL-CTX @               s" cuCtxSetCurrent"          SL-SYM CALL1 drop
   s" /tmp/softmax.cubin" SL-PATH >CSTR
   SL-MOD P>N SL-PATH P>N s" cuModuleLoad"             SL-SYM CALL2 drop
   s" SOFTMAX_ROWS" SL-KN >CSTR
   SL-FUNC P>N SL-MOD @ SL-KN P>N s" cuModuleGetFunction" SL-SYM CALL3 drop ;

: SL-LAUNCH ( -- )                                    \ grid = 2 rows, block = 256
   2 4 256 PTX-ROW-LAUNCH-CHECK
   SL-DIN P>N 32          s" cuMemAlloc_v2"   SL-SYM CALL2 drop
   SL-DOUT P>N 32         s" cuMemAlloc_v2"   SL-SYM CALL2 drop
   SL-DIN @ SL-HIN P>N 32 s" cuMemcpyHtoD_v2" SL-SYM CALL3 drop
   4 SL-KV !
   SL-FUNC @ 256 1 1      s" cuFuncSetBlockShape" SL-SYM CALL4 drop
   SL-FUNC @ 20           s" cuParamSetSize"  SL-SYM CALL2 drop
   SL-FUNC @ 0  SL-DIN P>N 8   s" cuParamSetv" SL-SYM CALL4 drop
   SL-FUNC @ 8  SL-DOUT P>N 8  s" cuParamSetv" SL-SYM CALL4 drop
   SL-FUNC @ 16 SL-KV P>N 4    s" cuParamSetv" SL-SYM CALL4 drop
   SL-FUNC @ 2 1          s" cuLaunchGrid"    SL-SYM CALL3 drop
   0                      s" cuCtxSynchronize" SL-SYM CALL1 drop
   SL-HOUT P>N SL-DOUT @ 32 s" cuMemcpyDtoH_v2" SL-SYM CALL3 drop ;

: SL-RELEASE ( -- )
   SL-MOD @  s" cuModuleUnload"            SL-SYM CALL1 drop
   SL-DEV @  s" cuDevicePrimaryCtxRelease" SL-SYM CALL1 drop ;

\ within 2 ULP of golden (ex2.approx introduces <= 1 ULP on the exp path)
: SL-NEAR? ( n n -- bool )  - abs 2 <= ;

SL-PUT SL-SETUP SL-LAUNCH SL-RELEASE

T-RESET
\ row0 = softmax([1,2,3,4]) within 2 ULP of the CPU golden
0 SL-F32@ 1023627234 SL-NEAR? TTRUE
1 SL-F32@ 1035106489 SL-NEAR? TTRUE
2 SL-F32@ 1047695721 SL-NEAR? TTRUE
3 SL-F32@ 1059379089 SL-NEAR? TTRUE
\ row1 = softmax([1,1,1,1]) = [0.25,0.25,0.25,0.25] = 0x3E800000 exactly
4 SL-F32@ 1048576000 T=
5 SL-F32@ 1048576000 T=
6 SL-F32@ 1048576000 T=
7 SL-F32@ 1048576000 T=
T-REPORT
bye
