\ maki/lower-launch.f - upload/launch/readback for one lowered elementwise region.
\
\ CAD-PLAN section 2 (PTX -> cubin -> launch), device leg slice 1. Given a region
\ already analyzed by maki/lower-ew.f (LEW-ANALYZE) and a cubin assembled from its
\ REGION_<rid> kernel, LLA-RUN drives the launch: pack each region input's synthetic
\ host buffer (GA-IN-PTR, the executor's bound buffer after GA-BIND-SYNTH) f64->f32,
\ cuMemcpyHtoD, sentinel-fill the readback, launch (grid = ceil(n/256), block 256),
\ cuMemcpyDtoH, and F32->F64 unpack the device output into LLA-HOUT. Every readback
\ cell is poisoned before the copy-back and GUARD-checked after, so a dropped copy
\ fails closed (E-PTX-READBACK) rather than passing a golden on stale data.
\
\ v1 restriction (documented): every region input must be a model INPUT SLOT - a
\ region fed by a materialized producer in another region needs cross-region buffer
\ orchestration that arrives with the OPTIMIZE wiring slice; such an input fails
\ closed (E-LLA-INPUT). N above the launch arena fails closed (E-LLA-CAP). Fully
\ checked Habu via the typed CUDA bindings (maki/cuda-driver.f). maki -> habu only;
\ lower-launch owns -5180..-5181.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/ptx/header.f
require lib/ptx/launch.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/sentinel.f
require maki/array.f
require maki/cuda-driver.f
require maki/golden-artifact.f
require maki/lower-ew.f

-5180 constant E-LLA-INPUT   \ a region input is not a model input slot (v1: slots only)
-5181 constant E-LLA-CAP     \ region element count exceeds the launch arena capacity

package MAKI

4    constant LLA-MAX-IN     \ mirrors lower-ew LEW-MAX-IN
4096 constant LLA-NCAP       \ max elements per buffer (16 KB f32)
256  constant LLA-BLOCK      \ launch block size

create LLA-HIN  LLA-MAX-IN LLA-NCAP * 4 * allot   \ K packed-f32 input buffers (bytes)
create LLA-HRB  LLA-NCAP 4 * allot                 \ device readback (packed f32 bytes)
create LLA-HOUT LLA-NCAP cells allot               \ unpacked device output (f64 cells)
create LLA-DBUF LLA-MAX-IN 1 + cells allot         \ devptr store: K inputs then output
variable LLA-NVAR                                   \ n as a u32 param cell
create LLA-FN   40 allot                            \ "REGION_<rid>" cstring
create LLA-PATH FS-PATH-CAP allot                   \ cubin path cstring
create LLA-CUBIN FS-PATH-CAP allot  variable LLA-CUBIN-U   \ cubin path (set by the tool)
variable LLA-DEV variable LLA-CTX variable LLA-MOD variable LLA-FUNC

\ ---- cubin path (the device tool assembles then hands the path here) --------
: LLA-CUBIN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a LLA-CUBIN u BYTE-COPY  u LLA-CUBIN-U ! ;
: LLA-CUBIN$ ( -- ptr u8 n )  LLA-CUBIN LLA-CUBIN-U @ ;

private

: LLA-HIN-I  ( n -- ptr a )  LLA-NCAP 4 * *  LLA-HIN + ;   \ i-th input byte buffer
: LLA-DBUF-I ( n -- ptr a )  cells LLA-DBUF + ;             \ i-th devptr cell

\ region input i -> its model-input slot (fail closed on a non-slot input)
: LLA-SLOT ( n -- n ) {: i:n :}
   i LEW-IN-REF@ {: ref:n :}
   ref MIR-REF-INPUT? 0= if E-LLA-INPUT throw then
   ref MIR-REF-SLOT ;

\ pack slot i's synthetic host f64 buffer (executor-bound) into LLA-HIN[i] as f32
: LLA-PACK-INPUT ( n -- ) {: i:n :}
   i LLA-SLOT GA-IN-PTR  LEW-ELEMS  i LLA-HIN-I  F32-PACK ;

: LLA-FNAME ( n -- ) {: rid:n :}                 \ build the REGION_<rid> cstring
   SB-RESET s" REGION_" SB-APPEND rid SB-INT  SB$ LLA-FN >CSTR ;

: LLA-SETUP ( -- )
   CUDA:OPEN
   0 CUDA:CUINIT CUDA:RC0
   LLA-DEV 0 >IDX CUDA:CUDEVICEGET CUDA:RC0
   LLA-CTX LLA-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   LLA-CTX @ >CUDA-CTX CUDA:CUCTXSETCURRENT CUDA:RC0
   LLA-CUBIN$ LLA-PATH >CSTR
   LLA-MOD LLA-PATH CUDA:CUMODULELOAD CUDA:RC0
   LLA-FUNC LLA-MOD @ >CUDA-MOD LLA-FN CUDA:CUMODULEGETFUNCTION CUDA:RC0 ;

: LLA-ALLOC-UPLOAD ( n -- ) {: obytes:n :}       \ alloc + copy inputs, alloc output
   LEW-NIN@ 0 ?do
      i LLA-DBUF-I obytes >LEN CUDA:CUMEMALLOC CUDA:RC0
      i LLA-DBUF-I @ >CUDA-DEVPTR  i LLA-HIN-I  obytes >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   loop
   LEW-NIN@ LLA-DBUF-I obytes >LEN CUDA:CUMEMALLOC CUDA:RC0 ;

: LLA-BIND-PARAMS ( -- )                          \ K input ptrs, output ptr, then n (u32)
   LLA-FUNC @ >CUDA-FN LLA-BLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   LLA-FUNC @ >CUDA-FN LEW-NIN@ 8 * 12 + >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   LEW-NIN@ 0 ?do
      LLA-FUNC @ >CUDA-FN  i 8 * >IDX  i LLA-DBUF-I 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   loop
   LLA-FUNC @ >CUDA-FN  LEW-NIN@ 8 * >IDX      LEW-NIN@ LLA-DBUF-I 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   LLA-FUNC @ >CUDA-FN  LEW-NIN@ 8 * 8 + >IDX  LLA-NVAR 4 >LEN CUDA:CUPARAMSETV CUDA:RC0 ;

: LLA-READBACK ( n n -- ) {: n:n obytes:n :}      \ launch, copy back, unpack (guarded)
   n PTX-LAUNCH-POSITIVE  LLA-BLOCK PTX-BLOCK-CHECK   \ reuse the launch-contract checks
   n LLA-NVAR !
   n LLA-BLOCK + 1 - LLA-BLOCK / {: grid:n :}
   LLA-BIND-PARAMS
   LLA-FUNC @ >CUDA-FN grid 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   LLA-HRB obytes PTXSENT:FILL
   LLA-HRB  LEW-NIN@ LLA-DBUF-I @ >CUDA-DEVPTR  obytes >LEN CUDA:CUMEMCPYDTOH CUDA:RC0
   n 0 ?do  LLA-HRB i 4 * + SF-LD PTXSENT:GUARD F32>F64  LLA-HOUT i T-SET  loop ;

: LLA-RELEASE ( -- )
   LEW-NIN@ 0 ?do  i LLA-DBUF-I @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0  loop
   LEW-NIN@ LLA-DBUF-I @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   LLA-MOD @ >CUDA-MOD CUDA:CUMODULEUNLOAD CUDA:RC0
   LLA-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRELEASE CUDA:RC0 ;

public

\ LLA-RUN analyzes region rid, uploads its synthetic inputs, launches REGION_<rid>
\ from the cubin at LLA-CUBIN$, and unpacks the device output into LLA-HOUT.
\ GA-BIND-SYNTH must have run so GA-IN-PTR holds the (executor-bound) inputs.
: LLA-RUN ( n -- ) {: rid:n :}
   rid LEW-ANALYZE
   LEW-ELEMS {: n:n :}
   n LLA-NCAP > if E-LLA-CAP throw then
   n 4 * {: obytes:n :}
   LEW-NIN@ 0 ?do i LLA-PACK-INPUT loop
   rid LLA-FNAME
   LLA-SETUP
   obytes LLA-ALLOC-UPLOAD
   n obytes LLA-READBACK
   LLA-RELEASE ;

\ device output element (f64 = the widened device f32) after LLA-RUN
: LLA-OUT@ ( n -- r )  LLA-HOUT swap T-GET ;

end-package
