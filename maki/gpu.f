\ maki/gpu.f - run a maki tensor op on the GPU (the maki -> Habu-PTX lowering).
\
\ AXPY over float arrays: y[i] = a*x[i] + y[i], computed on the Orin via the
\ CHECKED SAXPY kernel (lib/ptx/...), with ARBITRARY float data marshalled through
\ F64>F32, and verified against the CPU. Fully checked Habu (no 0 set-check) via
\ the checked FFI (lib/ffi.f) + F64>F32 (lib/ptx/cg.f). maki -> habu only.
\ Prereq: cubin at /tmp/saxpy.cubin (tools/ptx/saxpy-cg.f + ptxas).

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

: GFFI-OPEN ( -- )  s" libcuda.so.1" GLIB >CSTR  GLIB RTLD-NOW DLOPEN GH ! ;
: GSY ( ptr u8 n -- n )  GSYM >CSTR  GH @ GSYM DLSYM ;

: G-SETUP ( -- )
   GFFI-OPEN
   0                    s" cuInit"                   GSY CALL1 drop
   GDEV P>N 0           s" cuDeviceGet"              GSY CALL2 drop
   GCTX P>N GDEV @      s" cuDevicePrimaryCtxRetain" GSY CALL2 drop
   GCTX @              s" cuCtxSetCurrent"           GSY CALL1 drop
   s" /tmp/saxpy.cubin" GPATH >CSTR
   GMOD P>N GPATH P>N   s" cuModuleLoad"             GSY CALL2 drop
   s" SAXPY" GKN >CSTR
   GFUNC P>N GMOD @ GKN P>N s" cuModuleGetFunction"  GSY CALL3 drop ;

\ load element i of x and y from Habu floats into the host f32 buffers
: G-PUT ( r r n -- ) {: xv yv ix :}
   xv F64>F32 GHX ix F32!
   yv F64>F32 GHY ix F32! ;

: G-LAUNCH ( r -- )  {: a :}                          \ a = scalar; x,y already in GHX/GHY
   GN 4 *  {: bytes :}
   GDX P>N bytes        s" cuMemAlloc_v2"   GSY CALL2 drop
   GDY P>N bytes        s" cuMemAlloc_v2"   GSY CALL2 drop
   GDX @ GHX P>N bytes  s" cuMemcpyHtoD_v2" GSY CALL3 drop
   GDY @ GHY P>N bytes  s" cuMemcpyHtoD_v2" GSY CALL3 drop
   a F64>F32 GABITS !  GN GNVAR !
   GFUNC @ 256 1 1      s" cuFuncSetBlockShape" GSY CALL4 drop
   GFUNC @ 24           s" cuParamSetSize"  GSY CALL2 drop
   GFUNC @ 0  GDX P>N 8    s" cuParamSetv"  GSY CALL4 drop
   GFUNC @ 8  GDY P>N 8    s" cuParamSetv"  GSY CALL4 drop
   GFUNC @ 16 GABITS P>N 4 s" cuParamSetv"  GSY CALL4 drop
   GFUNC @ 20 GNVAR P>N 4  s" cuParamSetv"  GSY CALL4 drop
   GFUNC @ 1 1          s" cuLaunchGrid"    GSY CALL3 drop
   0                    s" cuCtxSynchronize" GSY CALL1 drop
   GHY P>N GDY @ bytes  s" cuMemcpyDtoH_v2" GSY CALL3 drop ;  \ ( dstHost srcDevice n )

: G-RELEASE ( -- )
   GMOD @  s" cuModuleUnload"            GSY CALL1 drop
   GDEV @  s" cuDevicePrimaryCtxRelease" GSY CALL1 drop ;

\ result element i (f32 bits) after the launch
: G-RESULT ( n -- n )  GHY swap F32@ ;
