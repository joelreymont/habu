\ bandwidth-lib.f - reusable Orin SAXPY-family kernel bandwidth runner.

require lib/errors.f
require lib/string.f
require lib/ffi.f
require tools/ptx/profile.f

package PTXBW

128 constant BW-PATH-CAP
$100000 constant BW-N
200 constant BW-ITERS
256 constant BW-BLOCK

create BW-LIB 16 allot
create BW-SYM-BUF 64 allot
create BW-PATH-BUF BW-PATH-CAP allot
create BW-KERNEL-BUF 32 allot
create BW-LABEL-BUF 32 allot

variable BW-PATH-U
variable BW-LABEL-U
variable BW-H
variable BW-DEV
variable BW-CTX
variable BW-MOD
variable BW-FUNC
variable BW-DX
variable BW-DY
variable BW-ABITS
variable BW-NV
variable BW-EPT

: BW-COPY! ( ptr u8 n ptr u8 ptr a -- )
   {: a u:n dst lenp:ptr :} \ ( a u dst lenp -- ) typed-local-lint: allow-bare-local - ptr roles.
   u BW-PATH-CAP > if E-FS-PATH throw then \ ( -- )
   a dst u BYTE-COPY \ ( -- )
   u lenp ! ; \ ( -- )

: BW-RC0 ( n -- )
   dup 0 <> if s" ptxbw: CUDA Driver call failed" 1 die then \ ( rc -- rc )
   drop ; \ ( rc -- )

: BW-NZ ( n -- )
   dup 0= if s" ptxbw: missing CUDA handle" 1 die then \ ( x -- x )
   drop ; \ ( x -- )

: BW-OPEN ( -- )
   s" libcuda.so.1" BW-LIB >CSTR \ ( -- )
   BW-LIB RTLD-NOW DLOPEN BW-H ! \ ( -- )
   BW-H @ BW-NZ ; \ ( -- )

: BW-SYM ( ptr u8 n -- n )
   BW-SYM-BUF >CSTR \ ( a u -- )
   BW-H @ BW-SYM-BUF DLSYM dup BW-NZ ; \ ( -- fn )

: BW-LABEL$ ( -- ptr u8 n )
   BW-LABEL-BUF BW-LABEL-U @ ;

: BW-PATH$ ( -- ptr u8 n )
   BW-PATH-BUF BW-PATH-U @ ;

public

: DEFAULTS ( -- )
   s" /tmp/saxpy.cubin" BW-PATH-BUF BW-PATH-U BW-COPY! \ ( -- )
   s" SAXPY" BW-LABEL-BUF BW-LABEL-U BW-COPY! \ ( -- )
   1 BW-EPT ! ; \ ( -- )

: CUBIN! ( ptr u8 n -- )
   BW-PATH-BUF BW-PATH-U BW-COPY! ; \ ( a u -- )

: LABEL! ( ptr u8 n -- )
   BW-LABEL-BUF BW-LABEL-U BW-COPY! ; \ ( a u -- )

: ELEMS-PER-THREAD! ( n -- )
   BW-EPT ! ; \ ( n -- )

private

: BW-SETUP ( -- )
   BW-OPEN \ ( -- )
   0                       s" cuInit"                   BW-SYM CALL1 BW-RC0 \ ( -- )
   BW-DEV P>N 0            s" cuDeviceGet"              BW-SYM CALL2 BW-RC0 \ ( -- )
   BW-CTX P>N BW-DEV @     s" cuDevicePrimaryCtxRetain" BW-SYM CALL2 BW-RC0 \ ( -- )
   BW-CTX @                s" cuCtxSetCurrent"          BW-SYM CALL1 BW-RC0 \ ( -- )
   BW-PATH$ nip 0= if s" ptxbw: cubin path not set" 1 die then \ ( -- )
   BW-PATH$ BW-PATH-BUF >CSTR \ ( -- )
   BW-MOD P>N BW-PATH-BUF P>N s" cuModuleLoad" BW-SYM CALL2 BW-RC0 \ ( -- )
   s" SAXPY" BW-KERNEL-BUF >CSTR \ ( -- )
   BW-FUNC P>N BW-MOD @ BW-KERNEL-BUF P>N s" cuModuleGetFunction" BW-SYM CALL3 BW-RC0 ; \ ( -- )

: BW-ALLOC ( -- )
   BW-DX P>N BW-N 4 *      s" cuMemAlloc_v2"  BW-SYM CALL2 BW-RC0 \ ( -- )
   BW-DY P>N BW-N 4 *      s" cuMemAlloc_v2"  BW-SYM CALL2 BW-RC0 \ ( -- )
   BW-DX @ 0 BW-N          s" cuMemsetD32_v2" BW-SYM CALL3 BW-RC0 \ ( -- )
   BW-DY @ 0 BW-N          s" cuMemsetD32_v2" BW-SYM CALL3 BW-RC0 ; \ ( -- )

: BW-PARAMS ( -- )
   $40000000 BW-ABITS ! \ ( -- )
   BW-N BW-NV ! \ ( -- )
   BW-FUNC @ BW-BLOCK 1 1       s" cuFuncSetBlockShape" BW-SYM CALL4 BW-RC0 \ ( -- )
   BW-FUNC @ 24                 s" cuParamSetSize" BW-SYM CALL2 BW-RC0 \ ( -- )
   BW-FUNC @ 0  BW-DX P>N 8     s" cuParamSetv" BW-SYM CALL4 BW-RC0 \ ( -- )
   BW-FUNC @ 8  BW-DY P>N 8     s" cuParamSetv" BW-SYM CALL4 BW-RC0 \ ( -- )
   BW-FUNC @ 16 BW-ABITS P>N 4  s" cuParamSetv" BW-SYM CALL4 BW-RC0 \ ( -- )
   BW-FUNC @ 20 BW-NV P>N 4     s" cuParamSetv" BW-SYM CALL4 BW-RC0 ; \ ( -- )

: BW-TILE-ELEMS ( -- n )
   BW-BLOCK BW-EPT @ * ; \ ( -- n )

: BW-GRID ( -- n )
   BW-N BW-TILE-ELEMS 1- + BW-TILE-ELEMS / ; \ ( -- n )

: BW-FIRE ( -- )
   BW-FUNC @ BW-GRID 1 s" cuLaunchGrid" BW-SYM CALL3 BW-RC0 ; \ ( -- )

: BW-SYNC ( -- )
   0 s" cuCtxSynchronize" BW-SYM CALL1 BW-RC0 ; \ ( -- )

: BW-RUN ( -- n )
   BW-FIRE BW-SYNC \ ( -- )
   mono-ns {: t0:n :} \ ( -- )
   BW-ITERS 0 ?do BW-FIRE loop \ ( -- )
   BW-SYNC \ ( -- )
   mono-ns t0 - ; \ ( -- ns )

: BW-RELEASE ( -- )
   BW-MOD @ 0 <> if BW-MOD @ s" cuModuleUnload" BW-SYM CALL1 BW-RC0 then \ ( -- )
   BW-DEV @ 0 <> if BW-DEV @ s" cuDevicePrimaryCtxRelease" BW-SYM CALL1 BW-RC0 then ; \ ( -- )

public

: REPORT ( -- )
   BW-SETUP BW-ALLOC BW-PARAMS \ ( -- )
   BW-RUN {: ns:n :} \ ( -- )
   BW-RELEASE \ ( -- )
   BW-N BW-ITERS PTXPROF:TRIAD-BYTES {: by:n :} \ ( -- )
   BW-N BW-ITERS PTXPROF:SAXPY-FLOPS {: fl:n :} \ ( -- )
   s" kernel=" type BW-LABEL$ type s"  n=" type BW-N . s"  iters=" type BW-ITERS . cr \ ( -- )
   s" block=" type BW-BLOCK . s"  elems_per_thread=" type BW-EPT @ . s"  elapsed_ns=" type ns . cr \ ( -- )
   by fl ns PTXPROF:REPORT-METRICS ; \ ( -- )

end-package
