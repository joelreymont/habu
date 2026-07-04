\ sum-launch.f - Orin device proof for the checked SUM-ROWS kernel.
\
\ Fully checked Habu via lib/ffi.f. Self-contained: spawns bin/hb to emit the
\ checked SUM-ROWS kernel (tools/ptx/sum-cg.f) to a PRIVATE PTX under a per-run
\ toolchain root, ptxas-assembles it, then launches on the Orin - no shared
\ /tmp/sum.cubin. Load after lib/test.f, lib/ptx/header.f, lib/ptx/launch.f,
\ lib/ffi.f, and f32 marshalling helpers.

require lib/ptx/toolchain.f
require lib/ptx/sentinel.f

create RS-LIB 16 allot  create RS-NM 64 allot  create RS-PATH 64 allot  create RS-KN 32 allot
create RS-HIN 32 allot  create RS-HOUT 32 allot
create RS-OUT $4000 allot  create RS-ERR $1000 allot
create RS-QO  $1000 allot  create RS-QE  $1000 allot
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

\ spawn bin/hb to emit the checked SUM-ROWS kernel to the private PTX
: RS-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"        >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"           >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"        >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/collective.f" >LEN PROC-ARGV+  s" tools/ptx/sum-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  RS-OUT $4000 >LEN  RS-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   RS-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ nonzero emit rc -> surface stderr, throw
   PTXTC:PTX$ RS-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

: RS-PTXAS ( -- n )
   RS-QO $1000 >LEN RS-QE $1000 >LEN PTXTC:ASSEMBLE ;

\ Two NONUNIFORM rows so an index/broadcast bug cannot pass on all-equal data:
\   row0 = [1,2,3,4] -> sum 10.0 (0x41200000)
\   row1 = [2,3,4,5] -> sum 14.0 (14 = 1.75*2^3: exp 130=0x82, mant .75=0x600000 -> 0x41600000)
: RS-PUT ( -- )
   1.0 F64>F32 RS-HIN 0 RS-F32!  2.0 F64>F32 RS-HIN 1 RS-F32!
   3.0 F64>F32 RS-HIN 2 RS-F32!  4.0 F64>F32 RS-HIN 3 RS-F32!
   2.0 F64>F32 RS-HIN 4 RS-F32!  3.0 F64>F32 RS-HIN 5 RS-F32!
   4.0 F64>F32 RS-HIN 6 RS-F32!  5.0 F64>F32 RS-HIN 7 RS-F32! ;

: RS-SETUP ( -- )
   RS-OPEN
   0                       s" cuInit"                   RS-SYM CALL1 drop
   RS-DEV P>N 0            s" cuDeviceGet"              RS-SYM CALL2 drop
   RS-CTX P>N RS-DEV @     s" cuDevicePrimaryCtxRetain" RS-SYM CALL2 drop
   RS-CTX @               s" cuCtxSetCurrent"          RS-SYM CALL1 drop
   PTXTC:CUBIN$ RS-PATH >CSTR
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

: SUM-MAIN ( -- )
   T-RESET
   s" habu-ptx-sum" PTXTC:PREPARE
   RS-EMIT drop
   RS-PTXAS PTXTC:ASM-REPORT 0 T=                  \ surface ptxas stderr before the assert
   RS-PUT RS-SETUP RS-LAUNCH RS-RELEASE
   PTXTC:CLEAN
   0 RS-F32@ PTXSENT:GUARD $41200000 T=            \ row0 sum = 10.0
   1 RS-F32@ PTXSENT:GUARD $41200000 T=
   2 RS-F32@ PTXSENT:GUARD $41200000 T=
   3 RS-F32@ PTXSENT:GUARD $41200000 T=
   4 RS-F32@ PTXSENT:GUARD $41600000 T=            \ row1 sum = 14.0
   5 RS-F32@ PTXSENT:GUARD $41600000 T=
   6 RS-F32@ PTXSENT:GUARD $41600000 T=
   7 RS-F32@ PTXSENT:GUARD $41600000 T=
   T-REPORT ;

SUM-MAIN
