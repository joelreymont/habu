\ maki/lower-launch.f - upload/launch/readback for one lowered fusion region.
\
\ CAD-PLAN section 2 (PTX -> cubin -> launch), device legs slice 1 (elementwise),
\ slice 2 (row-reduce), and slice 3 (matmul/linear). Given a region already analyzed by its owning lowering pass and
\ a cubin assembled from its REGION_<rid> kernel, the RUN word drives the launch: pack
\ each region input's synthetic host buffer (GA-IN-PTR, the executor's bound buffer after
\ GA-BIND-SYNTH) f64->f32, cuMemcpyHtoD, sentinel-fill the readback, launch, cuMemcpyDtoH,
\ and F32->F64 unpack the device output into LLA-HOUT. Every readback cell is poisoned
\ before the copy-back and GUARD-checked after, so a dropped copy fails closed
\ (E-PTX-READBACK) rather than passing a golden on stale data.
\
\ The kernel SHAPES differ in the launch grid, the u32 kernel params, and (matmul only)
\ per-input buffer sizes, so the CUDA plumbing (setup / alloc+upload / bind /
\ launch+readback / release) is factored over launch-local STAGING (LLA-NIN / LLA-ELEMS /
\ LLA-OUT-NODE / LLA-IN-REF), populated by the shape's analyzer before the shared core
\ runs; the guarded readback (LLA-READBACK) is shared by every shape:
\   LLA-RUN  (elementwise, maki/lower-ew.f):  grid = ceil(N/256), p_n = N = rows*cols.
\   LRED-RUN (row-reduce,  maki/lower-red.f):  grid = rows,        p_k = cols (one block/row).
\   LMM-RUN  (matmul,      maki/lower-mm.f):   2D grid = (ceil(N/16),ceil(M/16)), block
\            16x16, params p_m/p_n/p_k = M/N/K. The contraction operands (A MxK, B KxN,
\            optional bias 1xN) have DIFFERENT element counts, so the matmul upload path
\            sizes each device buffer from LLA-IN-ELEMS instead of the uniform obytes the
\            elementwise/row-reduce shapes share.
\ All name the kernel REGION_<rid> and read back rows*cols f32 elements. The golden
\ (maki/lower-golden.f) reads the staged out-node + element count.
\
\ v1 restriction (documented): every region input must be a model INPUT SLOT - a region fed
\ by a materialized producer in another region needs cross-region buffer orchestration that
\ arrives with the OPTIMIZE wiring slice; such an input fails closed (E-LLA-INPUT). rows*cols
\ above the launch arena fails closed (E-LLA-CAP). Fully checked Habu via the typed CUDA
\ bindings (maki/cuda-driver.f). maki -> habu only; lower-launch owns -5180..-5181.

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
require maki/move-view.f
require maki/lower-ew.f
require maki/lower-red.f
require maki/lower-mm.f
require maki/lower-move.f

-5180 constant E-LLA-INPUT   \ a region input is not a model input slot (v1: slots only)
-5181 constant E-LLA-CAP     \ region element count exceeds the launch arena capacity

package MAKI

4    constant LLA-MAX-IN     \ mirrors lower-ew LEW-MAX-IN / lower-red LRED-MAX-IN
4096 constant LLA-NCAP       \ max elements per buffer (16 KB f32)
256  constant LLA-BLOCK      \ launch block size (both shapes)

create LLA-HIN  LLA-MAX-IN LLA-NCAP * 4 * allot   \ K packed-f32 input buffers (bytes)
create LLA-HRB  LLA-NCAP 4 * allot                 \ device readback (packed f32 bytes)
create LLA-HOUT LLA-NCAP cells allot               \ unpacked device output (f64 cells)
create LLA-DBUF LLA-MAX-IN 1 + cells allot         \ devptr store: K inputs then output
variable LLA-NVAR                                   \ the u32 kernel param cell (N or cols)
create LLA-FN   40 allot                            \ "REGION_<rid>" cstring
create LLA-PATH FS-PATH-CAP allot                   \ cubin path cstring
create LLA-CUBIN FS-PATH-CAP allot  variable LLA-CUBIN-U   \ cubin path (set by the tool)
variable LLA-DEV variable LLA-CTX variable LLA-MOD variable LLA-FUNC

16   constant LLA-MM-TILE                           \ matmul 16x16 output tile (block 256)

\ ---- launch-local staging (populated per shape; the shared core reads only these) ----
variable LLA-NIN                                    \ region input count
variable LLA-ELEMS                                  \ output element count (rows*cols)
variable LLA-OUT-NODE                               \ the region's materialized output node
create LLA-IN-REF LLA-MAX-IN cells allot            \ per-input operand ref (RESOLVED to an input slot)
create LLA-IN-ELEMS LLA-MAX-IN cells allot          \ per-input element count (heterogeneous buffers)
variable LLA-PM  variable LLA-PN  variable LLA-PK   \ matmul u32 kernel params (M, N, K)
variable LLA-CPA variable LLA-CPB                   \ movement copy-kernel u32 params (p_a, p_b)

\ ---- cubin path (the device tool assembles then hands the path here) --------
: LLA-CUBIN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a LLA-CUBIN u BYTE-COPY  u LLA-CUBIN-U ! ;
: LLA-CUBIN$ ( -- ptr u8 n )  LLA-CUBIN LLA-CUBIN-U @ ;

\ ---- staged accessors (the golden reads these regardless of shape) ----------
: LLA-OUT-NODE@ ( -- n )  LLA-OUT-NODE @ ;
: LLA-ELEMS@ ( -- n )     LLA-ELEMS @ ;

private

: LLA-HIN-I  ( n -- ptr a )  LLA-NCAP 4 * *  LLA-HIN + ;   \ i-th input byte buffer
: LLA-DBUF-I ( n -- ptr a )  cells LLA-DBUF + ;             \ i-th devptr cell

\ region input i -> its model-input slot (fail closed on a non-slot input)
: LLA-SLOT ( n -- n ) {: i:n :}
   LLA-IN-REF i cells + @ {: ref:n :}
   ref MIR-REF-INPUT? 0= if E-LLA-INPUT throw then
   ref MIR-REF-SLOT ;

\ every shape stages a per-input element count (LLA-IN-ELEMS); a pure EW/RED input is
\ the full region shape, a matmul operand its own MxK/KxN/1xN, and a folded movement input
\ its SOURCE buffer (bigger than the output when a slice offset is baked into the kernel).
: LLA-IN-ELEMS-I ( n -- n )  cells LLA-IN-ELEMS + @ ;

\ pack slot i's synthetic host f64 buffer (executor-bound) into LLA-HIN[i] as f32
: LLA-PACK-INPUT ( n -- ) {: i:n :}
   i LLA-SLOT GA-IN-PTR  i LLA-IN-ELEMS-I  i LLA-HIN-I  F32-PACK ;

\ resolve one region operand ref (a movement node folds to its source slot) into staging
: LLA-REF-ELEMS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT dup MIR-SLOT-ROWS@ swap MIR-SLOT-COLS@ *
   else ref dup MIR-ROWS@ swap MIR-COLS@ * then ;
: LLA-STAGE-IN ( n n -- ) {: i:n ref:n :}
   ref MVW-RESOLVE-SRC {: src:n :}
   src  LLA-IN-REF i cells + !
   src LLA-REF-ELEMS  LLA-IN-ELEMS i cells + ! ;

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

: LLA-ALLOC-UPLOAD ( n -- ) {: obytes:n :}       \ per-input alloc + copy, then output alloc
   LLA-NIN @ 0 ?do
      i LLA-IN-ELEMS-I 4 * {: ib:n :}
      i LLA-DBUF-I ib >LEN CUDA:CUMEMALLOC CUDA:RC0
      i LLA-DBUF-I @ >CUDA-DEVPTR  i LLA-HIN-I  ib >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   loop
   LLA-NIN @ LLA-DBUF-I obytes >LEN CUDA:CUMEMALLOC CUDA:RC0 ;

: LLA-BIND-PARAMS ( -- )                          \ K input ptrs, output ptr, then the u32 param
   LLA-FUNC @ >CUDA-FN LLA-BLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   LLA-FUNC @ >CUDA-FN LLA-NIN @ 8 * 12 + >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   LLA-NIN @ 0 ?do
      LLA-FUNC @ >CUDA-FN  i 8 * >IDX  i LLA-DBUF-I 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   loop
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * >IDX      LLA-NIN @ LLA-DBUF-I 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * 8 + >IDX  LLA-NVAR 4 >LEN CUDA:CUPARAMSETV CUDA:RC0 ;

\ sentinel-poison the readback, copy the output back, GUARD every cell, unpack to f64.
\ Shared by all shapes (the launch grid + param binding is the only per-shape difference).
: LLA-READBACK ( n -- ) {: obytes:n :}
   LLA-HRB obytes PTXSENT:FILL
   LLA-HRB  LLA-NIN @ LLA-DBUF-I @ >CUDA-DEVPTR  obytes >LEN CUDA:CUMEMCPYDTOH CUDA:RC0
   LLA-ELEMS @ 0 ?do  LLA-HRB i 4 * + SF-LD PTXSENT:GUARD F32>F64  LLA-HOUT i T-SET  loop ;

: LLA-LAUNCH ( n n -- ) {: grid:n obytes:n :}     \ launch the staged grid, copy back, unpack (guarded)
   LLA-BIND-PARAMS
   LLA-FUNC @ >CUDA-FN grid 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   obytes LLA-READBACK ;

: LLA-RELEASE ( -- )
   LLA-NIN @ 0 ?do  i LLA-DBUF-I @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0  loop
   LLA-NIN @ LLA-DBUF-I @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   LLA-MOD @ >CUDA-MOD CUDA:CUMODULEUNLOAD CUDA:RC0
   LLA-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRELEASE CUDA:RC0 ;

\ ---- shared execute over the staged region (upload + launch grid + readback) ----
: LLA-EXEC ( n n -- ) {: rid:n grid:n :}
   LLA-ELEMS @ LLA-NCAP > if E-LLA-CAP throw then
   LLA-NIN @ 0 ?do  i LLA-IN-ELEMS-I LLA-NCAP > if E-LLA-CAP throw then  loop
   LLA-ELEMS @ 4 * {: obytes:n :}
   LLA-NIN @ 0 ?do i LLA-PACK-INPUT loop
   rid LLA-FNAME
   LLA-SETUP
   obytes LLA-ALLOC-UPLOAD
   grid obytes LLA-LAUNCH
   LLA-RELEASE ;

\ ---- per-shape staging (copy the analyzer's facts into launch-local state) ----
\ each staging resolves per-input (a dissolved movement operand folds to its source slot +
\ the source element count; LLA-STAGE-IN). The kernel's baked base offset does the rest.
: LLA-STAGE-EW ( n -- ) {: rid:n :}
   rid LEW-ANALYZE
   LEW-NIN@ LLA-NIN !  LEW-ELEMS LLA-ELEMS !  LEW-OUT-NODE@ LLA-OUT-NODE !
   LEW-NIN@ 0 ?do  i  i LEW-IN-REF@  LLA-STAGE-IN  loop ;

: LLA-STAGE-RED ( n -- ) {: rid:n :}
   rid LRED-ANALYZE
   LRED-NIN@ LLA-NIN !  LRED-ELEMS LLA-ELEMS !  LRED-OUT-NODE@ LLA-OUT-NODE !
   LRED-NIN@ 0 ?do  i  i LRED-IN-REF@  LLA-STAGE-IN  loop ;

: LLA-STAGE-MM ( n -- ) {: rid:n :}
   rid LMM-ANALYZE
   LMM-NIN@ LLA-NIN !  LMM-ELEMS LLA-ELEMS !  LMM-OUT-NODE@ LLA-OUT-NODE !
   LMM-NIN@ 0 ?do  i  i LMM-IN-REF@  LLA-STAGE-IN  loop
   LMM-M@ LLA-PM !  LMM-N@ LLA-PN !  LMM-K@ LLA-PK ! ;

\ movement copy region (maki/lower-move.f): 1-2 buffer operands are already input slots
: LLA-STAGE-MV ( n -- ) {: rid:n :}
   rid LMV-ANALYZE
   LMV-NIN@ LLA-NIN !  LMV-ELEMS LLA-ELEMS !  LMV-OUT-NODE@ LLA-OUT-NODE !
   LMV-NIN@ 0 ?do
      i LMV-IN-REF@    LLA-IN-REF i cells + !
      i LMV-IN-ELEMS@  LLA-IN-ELEMS i cells + !
   loop
   LMV-PA@ LLA-CPA !  LMV-PB@ LLA-CPB ! ;

\ ---- matmul upload/bind/launch (heterogeneous input buffers + 3 u32 params + 2D grid) --
: LLA-BIND-PARAMS-MM ( -- )                       \ K input ptrs, output ptr, then M,N,K as u32
   LLA-FUNC @ >CUDA-FN LLA-MM-TILE LLA-MM-TILE 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   LLA-FUNC @ >CUDA-FN LLA-NIN @ 8 * 20 + >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   LLA-NIN @ 0 ?do
      LLA-FUNC @ >CUDA-FN  i 8 * >IDX  i LLA-DBUF-I 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   loop
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * >IDX       LLA-NIN @ LLA-DBUF-I 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * 8  + >IDX  LLA-PM 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * 12 + >IDX  LLA-PN 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * 16 + >IDX  LLA-PK 4 >LEN CUDA:CUPARAMSETV CUDA:RC0 ;

: LLA-LAUNCH-MM ( n n n -- ) {: gx:n gy:n obytes:n :}   \ 2D launch, copy back, unpack (guarded)
   LLA-BIND-PARAMS-MM
   LLA-FUNC @ >CUDA-FN gx gy CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   obytes LLA-READBACK ;

: LLA-EXEC-MM ( n n n -- ) {: rid:n gx:n gy:n :}
   LLA-ELEMS @ LLA-NCAP > if E-LLA-CAP throw then
   LLA-NIN @ 0 ?do  i LLA-IN-ELEMS-I LLA-NCAP > if E-LLA-CAP throw then  loop
   LLA-ELEMS @ 4 * {: obytes:n :}
   LLA-NIN @ 0 ?do i LLA-PACK-INPUT loop
   rid LLA-FNAME
   LLA-SETUP
   obytes LLA-ALLOC-UPLOAD
   gx gy obytes LLA-LAUNCH-MM
   LLA-RELEASE ;

\ ---- movement copy-kernel launch (1-2 buffers + p_a/p_b/p_n u32 + 1D grid) ------
: LLA-BIND-PARAMS-MV ( -- )                       \ K input ptrs, output ptr, then a,b,n as u32
   LLA-FUNC @ >CUDA-FN LLA-BLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   LLA-FUNC @ >CUDA-FN LLA-NIN @ 8 * 20 + >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   LLA-NIN @ 0 ?do
      LLA-FUNC @ >CUDA-FN  i 8 * >IDX  i LLA-DBUF-I 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   loop
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * >IDX       LLA-NIN @ LLA-DBUF-I 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * 8  + >IDX  LLA-CPA 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * 12 + >IDX  LLA-CPB 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   LLA-FUNC @ >CUDA-FN  LLA-NIN @ 8 * 16 + >IDX  LLA-NVAR 4 >LEN CUDA:CUPARAMSETV CUDA:RC0 ;

: LLA-LAUNCH-MV ( n n -- ) {: grid:n obytes:n :}  \ 1D launch, copy back, unpack (guarded)
   LLA-BIND-PARAMS-MV
   LLA-FUNC @ >CUDA-FN grid 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   obytes LLA-READBACK ;

: LLA-EXEC-MV ( n n -- ) {: rid:n grid:n :}
   LLA-ELEMS @ LLA-NCAP > if E-LLA-CAP throw then
   LLA-NIN @ 0 ?do  i LLA-IN-ELEMS-I LLA-NCAP > if E-LLA-CAP throw then  loop
   LLA-ELEMS @ 4 * {: obytes:n :}
   LLA-NIN @ 0 ?do i LLA-PACK-INPUT loop
   rid LLA-FNAME  LLA-SETUP  obytes LLA-ALLOC-UPLOAD
   grid obytes LLA-LAUNCH-MV
   LLA-RELEASE ;

public

\ LLA-RUN analyzes elementwise region rid, uploads its synthetic inputs, launches the
\ REGION_<rid> flat kernel (grid = ceil(N/256)) from LLA-CUBIN$, and unpacks the device
\ output into LLA-HOUT. GA-BIND-SYNTH must have run so GA-IN-PTR holds the inputs.
: LLA-RUN ( n -- ) {: rid:n :}
   rid LLA-STAGE-EW
   LLA-ELEMS @ {: n:n :}
   n PTX-LAUNCH-POSITIVE  LLA-BLOCK PTX-BLOCK-CHECK
   n LLA-NVAR !                                    \ flat u32 param p_n = N
   n LLA-BLOCK + 1 - LLA-BLOCK / {: grid:n :}
   rid grid LLA-EXEC ;

\ LRED-RUN analyzes row-reduce region rid and launches its REGION_<rid> block-per-row
\ kernel (grid = rows, p_k = cols). Same synthetic-input upload + guarded readback.
: LRED-RUN ( n -- ) {: rid:n :}
   rid LLA-STAGE-RED
   LRED-ROWS@ {: rows:n :}  LRED-COLS@ {: cols:n :}
   rows cols LLA-BLOCK PTX-ROW-LAUNCH-CHECK
   cols LLA-NVAR !                                 \ row u32 param p_k = cols
   rid rows LLA-EXEC ;

\ LMM-RUN analyzes matmul/linear region rid and launches its REGION_<rid> tiled-GEMM
\ kernel (2D grid ceil(N/16) x ceil(M/16), block 16x16, params M/N/K). Contraction
\ operands (A, B, [bias]) upload at their own sizes; same guarded readback into LLA-HOUT.
: LMM-RUN ( n -- ) {: rid:n :}
   rid LLA-STAGE-MM
   LLA-PM @ PTX-LAUNCH-POSITIVE  LLA-PN @ PTX-LAUNCH-POSITIVE  LLA-PK @ PTX-LAUNCH-POSITIVE
   LLA-PN @ LLA-MM-TILE + 1 - LLA-MM-TILE / {: gx:n :}      \ ceil(N/16)
   LLA-PM @ LLA-MM-TILE + 1 - LLA-MM-TILE / {: gy:n :}      \ ceil(M/16)
   rid gx gy LLA-EXEC-MM ;

\ LMV-RUN analyzes a materialized movement region (maki/lower-move.f) and launches its
\ REGION_<rid> copy kernel (grid = ceil(N/256), one elem/thread, p_a/p_b/p_n u32). Buffer
\ operands upload at their own sizes (concat A/B, gather source+index differ). Same guarded
\ readback into LLA-HOUT.
: LMV-RUN ( n -- ) {: rid:n :}
   rid LLA-STAGE-MV
   LLA-ELEMS @ {: n:n :}
   n PTX-LAUNCH-POSITIVE  LLA-BLOCK PTX-BLOCK-CHECK
   n LLA-NVAR !                                    \ copy u32 param p_n = N
   n LLA-BLOCK + 1 - LLA-BLOCK / {: grid:n :}
   rid grid LLA-EXEC-MV ;

\ device output element (f64 = the widened device f32) after LLA-RUN / LRED-RUN / LMM-RUN
: LLA-OUT@ ( n -- r )  LLA-HOUT swap T-GET ;

end-package
