\ sum-launch.f - Orin device proof for the checked SUM-ROWS kernel.
\
\ Fully checked Habu via lib/ffi.f. Prereq: /tmp/sum.cubin produced from
\ tools/ptx/sum-cg.f and ptxas -arch=sm_87. Load after lib/test.f,
\ lib/ptx/header.f, lib/ptx/launch.f, lib/ffi.f, and f32 marshalling helpers.

require lib/ptx/sentinel.f

create RS-LIB 16 allot  create RS-NM 64 allot  create RS-PATH 64 allot  create RS-KN 32 allot
create RS-HIN 32 allot  create RS-HOUT 32 allot
variable RS-H variable RS-DEV variable RS-CTX variable RS-MOD variable RS-FUNC
variable RS-DIN variable RS-DOUT variable RS-KV

: RS-F32! ( n ptr u8 n -- ) {: v:n buf:ptr idx:n :}
   idx 4 * {: o:n :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;

: RS-F32@ ( n -- n ) {: idx:n :}
   idx 4 * {: o:n :}
   RS-HOUT o + c@  RS-HOUT o 1 + + c@ 8 lshift or
   RS-HOUT o 2 + + c@ 16 lshift or  RS-HOUT o 3 + + c@ 24 lshift or ;

: RS-OPEN ( -- )
   s" libcuda.so.1" RS-LIB >CSTR  RS-LIB RTLD-NOW DLOPEN RS-H ! ;

: RS-SYM ( ptr u8 n -- n )
   RS-NM >CSTR  RS-H @ RS-NM DLSYM ;

: RS-PUT ( -- )
   1.0 F64>F32 RS-HIN 0 RS-F32!  2.0 F64>F32 RS-HIN 1 RS-F32!
   3.0 F64>F32 RS-HIN 2 RS-F32!  4.0 F64>F32 RS-HIN 3 RS-F32!
   1.0 F64>F32 RS-HIN 4 RS-F32!  1.0 F64>F32 RS-HIN 5 RS-F32!
   1.0 F64>F32 RS-HIN 6 RS-F32!  1.0 F64>F32 RS-HIN 7 RS-F32! ;

: RS-SETUP ( -- )
   RS-OPEN
   0                       s" cuInit"                   RS-SYM CALL1 drop
   RS-DEV P>N 0            s" cuDeviceGet"              RS-SYM CALL2 drop
   RS-CTX P>N RS-DEV @     s" cuDevicePrimaryCtxRetain" RS-SYM CALL2 drop
   RS-CTX @               s" cuCtxSetCurrent"          RS-SYM CALL1 drop
   s" /tmp/sum.cubin" RS-PATH >CSTR
   RS-MOD P>N RS-PATH P>N s" cuModuleLoad"             RS-SYM CALL2 drop
   s" SUM_ROWS" RS-KN >CSTR
   RS-FUNC P>N RS-MOD @ RS-KN P>N s" cuModuleGetFunction" RS-SYM CALL3 drop ;

: RS-LAUNCH ( -- )
   RS-HOUT 32 PTXSENT:FILL                        \ poison readback: dropped copy-back fails closed
   2 4 256 PTX-ROW-LAUNCH-CHECK
   RS-DIN P>N 32           s" cuMemAlloc_v2"   RS-SYM CALL2 drop
   RS-DOUT P>N 32          s" cuMemAlloc_v2"   RS-SYM CALL2 drop
   RS-DIN @ RS-HIN P>N 32  s" cuMemcpyHtoD_v2" RS-SYM CALL3 drop
   4 RS-KV !
   RS-FUNC @ 256 1 1       s" cuFuncSetBlockShape" RS-SYM CALL4 drop
   RS-FUNC @ 20            s" cuParamSetSize"  RS-SYM CALL2 drop
   RS-FUNC @ 0  RS-DIN P>N 8   s" cuParamSetv" RS-SYM CALL4 drop
   RS-FUNC @ 8  RS-DOUT P>N 8  s" cuParamSetv" RS-SYM CALL4 drop
   RS-FUNC @ 16 RS-KV P>N 4    s" cuParamSetv" RS-SYM CALL4 drop
   RS-FUNC @ 2 1           s" cuLaunchGrid"    RS-SYM CALL3 drop
   0                       s" cuCtxSynchronize" RS-SYM CALL1 drop
   RS-HOUT P>N RS-DOUT @ 32 s" cuMemcpyDtoH_v2" RS-SYM CALL3 drop ;

: RS-RELEASE ( -- )
   RS-MOD @  s" cuModuleUnload"            RS-SYM CALL1 drop
   RS-DEV @  s" cuDevicePrimaryCtxRelease" RS-SYM CALL1 drop ;

RS-PUT RS-SETUP RS-LAUNCH RS-RELEASE

T-RESET
0 RS-F32@ PTXSENT:GUARD $41200000 T=
1 RS-F32@ PTXSENT:GUARD $41200000 T=
2 RS-F32@ PTXSENT:GUARD $41200000 T=
3 RS-F32@ PTXSENT:GUARD $41200000 T=
4 RS-F32@ PTXSENT:GUARD $40800000 T=
5 RS-F32@ PTXSENT:GUARD $40800000 T=
6 RS-F32@ PTXSENT:GUARD $40800000 T=
7 RS-F32@ PTXSENT:GUARD $40800000 T=
T-REPORT
bye
