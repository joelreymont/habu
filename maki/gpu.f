\ maki/gpu.f - run a maki tensor op on the GPU (the maki -> Habu-PTX lowering).
\
\ AXPY over float arrays: y[i] = a*x[i] + y[i], computed on the Orin via the
\ CHECKED SAXPY kernel (lib/ptx/...), with ARBITRARY float data marshalled through
\ F64>F32, and verified against the CPU. Fully checked Habu (no 0 set-check) via
\ the checked FFI (lib/ffi.f) + F64>F32 (lib/ptx/cg.f). maki -> habu only.
\ Prereq: cubin at /tmp/saxpy.cubin (tools/ptx/saxpy-cg.f + ptxas).

require maki/cuda-driver.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/sentinel.f

4 constant GN                       \ vector length (demo)
create GPATH 64 allot
create GKN  32 allot
create GHX  32 allot                \ host x as packed f32 (GN*4 bytes)
create GHY  32 allot                \ host y as packed f32
variable GDEV variable GCTX variable GMOD variable GFUNC
variable GDX variable GDY variable GABITS variable GNVAR

\ pack/read a 32-bit value at element idx (4-byte stride, little-endian)
: F32! ( n ptr u8 n -- ) {: v buf idx :}
   idx 4 *  {: o :}
   v             $FF and  buf o     + c!
   v 8 rshift    $FF and  buf o 1 + + c!
   v 16 rshift   $FF and  buf o 2 + + c!
   v 24 rshift   $FF and  buf o 3 + + c! ;
: F32@ ( ptr u8 n -- n ) {: buf idx :}
   idx 4 *  {: o :}
   buf o     + c@
   buf o 1 + + c@  8  lshift or
   buf o 2 + + c@  16 lshift or
   buf o 3 + + c@  24 lshift or ;

: G-SETUP ( -- )
   CUDA:OPEN
   0 CUDA:CUINIT CUDA:RC0
   GDEV 0 >IDX CUDA:CUDEVICEGET CUDA:RC0
   GCTX GDEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   GCTX @ >CUDA-CTX CUDA:CUCTXSETCURRENT CUDA:RC0
   s" /tmp/saxpy.cubin" GPATH >CSTR
   GMOD GPATH CUDA:CUMODULELOAD CUDA:RC0
   s" SAXPY" GKN >CSTR
   GFUNC GMOD @ >CUDA-MOD GKN CUDA:CUMODULEGETFUNCTION CUDA:RC0 ;

\ load element i of x and y from Habu floats into the host f32 buffers
: G-PUT ( r r n -- ) {: xv yv ix :}
   xv F64>F32 GHX ix F32!
   yv F64>F32 GHY ix F32! ;

: G-LAUNCH ( r -- )  {: a :}                          \ a = scalar; x,y already in GHX/GHY
   GN 4 *  {: bytes :}
   GDX bytes >LEN CUDA:CUMEMALLOC CUDA:RC0
   GDY bytes >LEN CUDA:CUMEMALLOC CUDA:RC0
   GDX @ >CUDA-DEVPTR GHX bytes >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   GDY @ >CUDA-DEVPTR GHY bytes >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   a F64>F32 GABITS !  GN GNVAR !
   GFUNC @ >CUDA-FN 256 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   GFUNC @ >CUDA-FN 24 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   GFUNC @ >CUDA-FN 0 >IDX  GDX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   GFUNC @ >CUDA-FN 8 >IDX  GDY 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   GFUNC @ >CUDA-FN 16 >IDX GABITS 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   GFUNC @ >CUDA-FN 20 >IDX GNVAR 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   GFUNC @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   GHY bytes PTXSENT:FILL                                \ poison before copy-back (y already on device)
   GHY GDY @ >CUDA-DEVPTR bytes >LEN CUDA:CUMEMCPYDTOH CUDA:RC0 ;

: G-RELEASE ( -- )
   GDX @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   GDY @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   GMOD @ >CUDA-MOD CUDA:CUMODULEUNLOAD CUDA:RC0
   GDEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRELEASE CUDA:RC0 ;

\ result element i (f32 bits) after the launch
: G-RESULT ( n -- n )  GHY swap F32@ PTXSENT:GUARD ;

\ tensor SGD step on the GPU: w[i] -= lr*g[i], lowered onto the SAXPY kernel
\ (a = -lr, x = grad, y = weight, so a*x+y = w - lr*g). Put grad as x and weight
\ as y via G-PUT, then G-SGD; G-RESULT i is the updated weight. Matches maki/array.f
\ T-SGD! on the f32-marshalled inputs - the optimizer step runs on device.
: G-SGD ( r -- )  fnegate G-LAUNCH ;
