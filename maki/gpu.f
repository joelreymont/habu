\ maki/gpu.f - run a maki tensor op on the GPU (the maki -> Habu-PTX lowering).
\
\ AXPY over float arrays: y[i] = a*x[i] + y[i], computed on the Orin via the
\ CHECKED SAXPY kernel (lib/ptx/...), with ARBITRARY float data marshalled through
\ F64>F32, and verified against the CPU. Fully checked Habu (no 0 set-check) via
\ the checked FFI (lib/ffi.f) + F64>F32 (lib/ptx/cg.f). maki -> habu only.
\ Prereq: cubin at /tmp/saxpy.cubin (tools/ptx/saxpy-cg.f + ptxas).

require maki/cuda-types.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f

4 constant GN                       \ vector length (demo)
create GLIB 16 allot
create GSYM 64 allot
create GPATH 64 allot
create GKN  32 allot
create GHX  32 allot                \ host x as packed f32 (GN*4 bytes)
create GHY  32 allot                \ host y as packed f32
variable GH  variable GDEV variable GCTX variable GMOD variable GFUNC
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

: GFFI-OPEN ( -- )
   s" libcuda.so.1" GLIB >CSTR
   GLIB RTLD-NOW DLOPEN CUDA-HANDLE0 GH ! ;
: GSY ( ptr u8 n -- n )  GSYM >CSTR  GH @ GSYM DLSYM ;

FFI: CUINIT ( n -- rc ) GSY cuInit FFI;
FFI: CUDEVICEGET ( ptr a idx -- rc ) GSY cuDeviceGet FFI;
FFI: CUDEVICEPRIMARYCTXRETAIN ( ptr a cuda-dev -- rc ) GSY cuDevicePrimaryCtxRetain FFI;
FFI: CUCTXSETCURRENT ( cuda-ctx -- rc ) GSY cuCtxSetCurrent FFI;
FFI: CUMODULELOAD ( ptr a ptr u8 -- rc ) GSY cuModuleLoad FFI;
FFI: CUMODULEGETFUNCTION ( ptr a cuda-mod ptr u8 -- rc ) GSY cuModuleGetFunction FFI;
FFI: CUMEMALLOC ( ptr a len -- rc ) GSY cuMemAlloc_v2 FFI;
FFI: CUMEMFREE ( cuda-devptr -- rc ) GSY cuMemFree_v2 FFI;
FFI: CUMEMCPYHTOD ( cuda-devptr ptr u8 len -- rc ) GSY cuMemcpyHtoD_v2 FFI;
FFI: CUMEMCPYDTOH ( ptr u8 cuda-devptr len -- rc ) GSY cuMemcpyDtoH_v2 FFI;
FFI: CUFUNCSETBLOCKSHAPE ( cuda-fn n n n -- rc ) GSY cuFuncSetBlockShape FFI;
FFI: CUPARAMSETSIZE ( cuda-fn len -- rc ) GSY cuParamSetSize FFI;
FFI: CUPARAMSETV ( cuda-fn idx ptr u8 len -- rc ) GSY cuParamSetv FFI;
FFI: CULAUNCHGRID ( cuda-fn n n -- rc ) GSY cuLaunchGrid FFI;
FFI: CUCTXSYNCHRONIZE ( -- rc ) GSY cuCtxSynchronize FFI;
FFI: CUMODULEUNLOAD ( cuda-mod -- rc ) GSY cuModuleUnload FFI;
FFI: CUDEVICEPRIMARYCTXRELEASE ( cuda-dev -- rc ) GSY cuDevicePrimaryCtxRelease FFI;

: G-SETUP ( -- )
   GFFI-OPEN
   0 CUINIT CUDA-RC0
   GDEV 0 >IDX CUDEVICEGET CUDA-RC0
   GCTX GDEV @ >CUDA-DEV CUDEVICEPRIMARYCTXRETAIN CUDA-RC0
   GCTX @ >CUDA-CTX CUCTXSETCURRENT CUDA-RC0
   s" /tmp/saxpy.cubin" GPATH >CSTR
   GMOD GPATH CUMODULELOAD CUDA-RC0
   s" SAXPY" GKN >CSTR
   GFUNC GMOD @ >CUDA-MOD GKN CUMODULEGETFUNCTION CUDA-RC0 ;

\ load element i of x and y from Habu floats into the host f32 buffers
: G-PUT ( r r n -- ) {: xv yv ix :}
   xv F64>F32 GHX ix F32!
   yv F64>F32 GHY ix F32! ;

: G-LAUNCH ( r -- )  {: a :}                          \ a = scalar; x,y already in GHX/GHY
   GN 4 *  {: bytes :}
   GDX bytes >LEN CUMEMALLOC CUDA-RC0
   GDY bytes >LEN CUMEMALLOC CUDA-RC0
   GDX @ >CUDA-DEVPTR GHX bytes >LEN CUMEMCPYHTOD CUDA-RC0
   GDY @ >CUDA-DEVPTR GHY bytes >LEN CUMEMCPYHTOD CUDA-RC0
   a F64>F32 GABITS !  GN GNVAR !
   GFUNC @ >CUDA-FN 256 1 1 CUFUNCSETBLOCKSHAPE CUDA-RC0
   GFUNC @ >CUDA-FN 24 >LEN CUPARAMSETSIZE CUDA-RC0
   GFUNC @ >CUDA-FN 0 >IDX  GDX 8 >LEN CUPARAMSETV CUDA-RC0
   GFUNC @ >CUDA-FN 8 >IDX  GDY 8 >LEN CUPARAMSETV CUDA-RC0
   GFUNC @ >CUDA-FN 16 >IDX GABITS 4 >LEN CUPARAMSETV CUDA-RC0
   GFUNC @ >CUDA-FN 20 >IDX GNVAR 4 >LEN CUPARAMSETV CUDA-RC0
   GFUNC @ >CUDA-FN 1 1 CULAUNCHGRID CUDA-RC0
   CUCTXSYNCHRONIZE CUDA-RC0
   GHY GDY @ >CUDA-DEVPTR bytes >LEN CUMEMCPYDTOH CUDA-RC0 ;

: G-RELEASE ( -- )
   GDX @ >CUDA-DEVPTR CUMEMFREE CUDA-RC0
   GDY @ >CUDA-DEVPTR CUMEMFREE CUDA-RC0
   GMOD @ >CUDA-MOD CUMODULEUNLOAD CUDA-RC0
   GDEV @ >CUDA-DEV CUDEVICEPRIMARYCTXRELEASE CUDA-RC0 ;

\ result element i (f32 bits) after the launch
: G-RESULT ( n -- n )  GHY swap F32@ ;

\ tensor SGD step on the GPU: w[i] -= lr*g[i], lowered onto the SAXPY kernel
\ (a = -lr, x = grad, y = weight, so a*x+y = w - lr*g). Put grad as x and weight
\ as y via G-PUT, then G-SGD; G-RESULT i is the updated weight. Matches maki/array.f
\ T-SGD! on the f32-marshalled inputs - the optimizer step runs on device.
: G-SGD ( r -- )  fnegate G-LAUNCH ;
