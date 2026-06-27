\ bandwidth.f - measure the checked SAXPY kernel's effective bandwidth on the Orin.
\
\ One column of the eval matrix (GB/s) for the Habu-PTX side. SAXPY moves 3*N*4
\ bytes per launch (read x, read y, write y). We time ITERS queued launches + one
\ sync over a large N and report GB/s = total_bytes / elapsed_ns (1 B/ns = 1 GB/s).
\ Fully checked Habu via lib/ffi.f (only P>N/N>P trusted). Prereq: /tmp/saxpy.cubin.
\ Load after lib/errors.f, lib/string.f, lib/ffi.f.

$100000 constant BW-N            \ 1,048,576 elements
200      constant BW-ITERS
create BW-LIB 16 allot  create BW-NM 64 allot  create BW-PATH 64 allot  create BW-KN 32 allot
variable BW-H variable BW-DEV variable BW-CTX variable BW-MOD variable BW-FUNC
variable BW-DX variable BW-DY variable BW-ABITS variable BW-NV

: BW-OPEN ( -- )  s" libcuda.so.1" BW-LIB >CSTR  BW-LIB RTLD-NOW DLOPEN BW-H ! ;
: BW-SYM ( ptr u8 n -- n )  BW-NM >CSTR  BW-H @ BW-NM DLSYM ;

: BW-SETUP ( -- )
   BW-OPEN
   0                       s" cuInit"                   BW-SYM CALL1 drop
   BW-DEV P>N 0            s" cuDeviceGet"              BW-SYM CALL2 drop
   BW-CTX P>N BW-DEV @     s" cuDevicePrimaryCtxRetain" BW-SYM CALL2 drop
   BW-CTX @               s" cuCtxSetCurrent"          BW-SYM CALL1 drop
   s" /tmp/saxpy.cubin" BW-PATH >CSTR
   BW-MOD P>N BW-PATH P>N s" cuModuleLoad"             BW-SYM CALL2 drop
   s" SAXPY" BW-KN >CSTR
   BW-FUNC P>N BW-MOD @ BW-KN P>N s" cuModuleGetFunction" BW-SYM CALL3 drop ;

: BW-ALLOC ( -- )                                 \ N*4 byte buffers, zeroed
   BW-DX P>N BW-N 4 *     s" cuMemAlloc_v2"  BW-SYM CALL2 drop
   BW-DY P>N BW-N 4 *     s" cuMemAlloc_v2"  BW-SYM CALL2 drop
   BW-DX @ 0 BW-N         s" cuMemsetD32_v2" BW-SYM CALL3 drop
   BW-DY @ 0 BW-N         s" cuMemsetD32_v2" BW-SYM CALL3 drop ;

: BW-PARAMS ( -- )                                \ a=2.0, n=N, block 256
   $40000000 BW-ABITS !  BW-N BW-NV !
   BW-FUNC @ 256 1 1      s" cuFuncSetBlockShape" BW-SYM CALL4 drop
   BW-FUNC @ 24           s" cuParamSetSize"  BW-SYM CALL2 drop
   BW-FUNC @ 0  BW-DX P>N 8    s" cuParamSetv" BW-SYM CALL4 drop
   BW-FUNC @ 8  BW-DY P>N 8    s" cuParamSetv" BW-SYM CALL4 drop
   BW-FUNC @ 16 BW-ABITS P>N 4 s" cuParamSetv" BW-SYM CALL4 drop
   BW-FUNC @ 20 BW-NV P>N 4    s" cuParamSetv" BW-SYM CALL4 drop ;

: BW-GRID ( -- n )  BW-N 255 + 256 / ;            \ ceil(N/256) blocks
: BW-FIRE ( -- )  BW-FUNC @ BW-GRID 1  s" cuLaunchGrid" BW-SYM CALL3 drop ;
: BW-SYNC ( -- )  0 s" cuCtxSynchronize" BW-SYM CALL1 drop ;

: BW-RUN ( -- n )                                 \ -> elapsed ns for BW-ITERS launches
   BW-FIRE BW-SYNC                                \ warm up
   mono-ns {: t0 :}
   BW-ITERS 0 ?do  BW-FIRE  loop
   BW-SYNC
   mono-ns t0 - ;

: BW-RELEASE ( -- )
   BW-MOD @  s" cuModuleUnload"            BW-SYM CALL1 drop
   BW-DEV @  s" cuDevicePrimaryCtxRelease" BW-SYM CALL1 drop ;

: BW-REPORT ( -- )
   BW-SETUP BW-ALLOC BW-PARAMS
   BW-RUN {: ns :}
   BW-RELEASE
   BW-ITERS 3 * BW-N * 4 * {: bytes :}            \ total bytes moved
   s" SAXPY N=" type BW-N . s"  iters=" type BW-ITERS . s"  elapsed_ns=" type ns . cr
   s" effective bandwidth (GB/s, x1000) = " type  bytes 1000 *  ns /  . cr
   s" (GB/s ~ " type  bytes ns /  . s" )" type cr ;

BW-REPORT
bye
