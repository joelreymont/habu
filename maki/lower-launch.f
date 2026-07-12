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
\ Single-region restriction (LOWER-GOLDEN path): a region run in isolation uploads every input
\ from its host synthetic buffer, so each input must be a model INPUT SLOT; a region fed by a
\ materialized producer in another region fails closed there (E-LLA-INPUT). That slots-only cap
\ is SUPERSEDED for the whole model by LOWER-MODEL-RUN (slice 5): it executes every region in
\ topo (materialized-node) order, keeps each region's output in a device buffer (MDL-BUF, keyed by
\ node), and BINDS that buffer for a downstream region whose input names the producer node instead
\ of uploading - so cross-region compute (LINEAR->GELU->LINEAR->RMSNORM) and a materialized movement
\ copy reading a producer buffer both run without the slots-only cap. Per-region cubins are
\ registered by MDL-CUBIN! (the device tool assembles REGION_<rid> per region). rows*cols above the
\ launch arena fails closed (E-LLA-CAP). Fully checked Habu via the typed CUDA bindings
\ (maki/cuda-driver.f). maki -> habu only; lower-launch owns -5180..-5184.

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

-5180 constant E-LLA-INPUT   \ a region input is not a model input slot (single-region path: slots only)
-5181 constant E-LLA-CAP     \ region element count exceeds the launch arena capacity
-5182 constant E-MDL-UNRESOLVED \ a region input names a producer node with no device buffer yet
-5183 constant E-MDL-NOOUT      \ the final model output node has no materialized device buffer
-5184 constant E-MDL-CUBIN      \ a region's cubin path was not registered before LOWER-MODEL-RUN

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
1 LAYOUT-BUFFER LLA-OUT-NODE CAD-KIND:node-id       \ the region's materialized output node (typed 1-slot cell)
create LLA-IN-REF LLA-MAX-IN cells allot            \ per-input operand ref (RESOLVED to an input slot)
create LLA-IN-ELEMS LLA-MAX-IN cells allot          \ per-input element count (heterogeneous buffers)
variable LLA-PM  variable LLA-PN  variable LLA-PK   \ matmul u32 kernel params (M, N, K)
variable LLA-CPA variable LLA-CPB                   \ movement copy-kernel u32 params (p_a, p_b)
: LLA-IN-REF! ( MIR:operand-ref n -- )  cells LLA-IN-REF + ! ;
: LLA-IN-REF@ ( n -- MIR:operand-ref )  cells LLA-IN-REF + @ ;
: LLA-OUT-NODE! ( CAD-KIND:node-id -- )  0 LLA-OUT-NODE ! ;
: LLA-STAGED-NODE@ ( -- CAD-KIND:node-id )  0 LLA-OUT-NODE @ ;

\ ---- cubin path (the device tool assembles then hands the path here) --------
: LLA-CUBIN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a LLA-CUBIN u BYTE-COPY  u LLA-CUBIN-U ! ;
: LLA-CUBIN$ ( -- ptr u8 n )  LLA-CUBIN LLA-CUBIN-U @ ;

\ ---- staged accessors (the golden reads these regardless of shape) ----------
: LLA-OUT-NODE@ ( -- CAD-KIND:node-id ) LLA-STAGED-NODE@ ;
: LLA-ELEMS@ ( -- n )     LLA-ELEMS @ ;

private

: LLA-HIN-I  ( n -- ptr a )  LLA-NCAP 4 * *  LLA-HIN + ;   \ i-th input byte buffer
: LLA-DBUF-I ( n -- ptr a )  cells LLA-DBUF + ;             \ i-th devptr cell

\ region input i -> its model-input slot (fail closed on a non-slot input)
: LLA-SLOT ( n -- MIR:input-slot ) {: i:n :}
   i LLA-IN-REF@ {: ref:MIR:operand-ref :}
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
: LLA-REF-ELEMS ( MIR:operand-ref -- n ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT?
   if ref MIR-REF-SLOT dup MIR-SLOT-ROWS@ swap MIR-SLOT-COLS@ SHAPE-ELEMS DIM-RAW
   else ref MIR-REF-NODE {: node:CAD-KIND:node-id :}
      node MIR-ROWS@ node MIR-COLS@ SHAPE-ELEMS DIM-RAW
   then ;
: LLA-STAGE-IN ( n MIR:operand-ref -- ) {: i:n ref:MIR:operand-ref :}
   ref MVW-RESOLVE-SRC {: src:MIR:operand-ref :}
   src i LLA-IN-REF!
   src LLA-REF-ELEMS  LLA-IN-ELEMS i cells + ! ;

\ RGN>RAW is the one kernel-name render boundary (REGION_<rid>)
: LLA-FNAME ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}   \ build the REGION_<rid> cstring
   SB-RESET s" REGION_" SB-APPEND rid RGN>RAW SB-INT  SB$ LLA-FN >CSTR ;

\ context lifecycle is split from module lifecycle: the whole-model run (LOWER-MODEL-RUN)
\ opens the context ONCE and loads/unloads a module PER region (device buffers are
\ context-scoped, so they persist across module loads); the single-region path composes both.
: LLA-CTX-OPEN ( -- )
   CUDA:OPEN
   0 CUDA:CUINIT CUDA:RC0
   LLA-DEV 0 >IDX CUDA:CUDEVICEGET CUDA:RC0
   LLA-CTX LLA-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   LLA-CTX @ >CUDA-CTX CUDA:CUCTXSETCURRENT CUDA:RC0 ;
: LLA-MOD-OPEN ( ptr u8 n -- )   \ load a cubin at the path; get the REGION_<...> fn (LLA-FN preset)
   LLA-PATH >CSTR
   LLA-MOD LLA-PATH CUDA:CUMODULELOAD CUDA:RC0
   LLA-FUNC LLA-MOD @ >CUDA-MOD LLA-FN CUDA:CUMODULEGETFUNCTION CUDA:RC0 ;
: LLA-MOD-CLOSE ( -- )  LLA-MOD @ >CUDA-MOD CUDA:CUMODULEUNLOAD CUDA:RC0 ;
: LLA-CTX-CLOSE ( -- )  LLA-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRELEASE CUDA:RC0 ;
: LLA-SETUP ( -- )  LLA-CTX-OPEN  LLA-CUBIN$ LLA-MOD-OPEN ;

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
   LLA-MOD-CLOSE  LLA-CTX-CLOSE ;

\ ---- shared execute over the staged region (upload + launch grid + readback) ----
: LLA-EXEC ( CAD-KIND:region n -- ) {: rid:CAD-KIND:region grid:n :}
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
: LLA-STAGE-EW ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LEW-ANALYZE
   LEW-NIN@ LLA-NIN ! LEW-ELEMS LLA-ELEMS ! LEW-OUT-NODE@ LLA-OUT-NODE!
   LEW-NIN@ 0 ?do  i  i LEW-IN-REF@  LLA-STAGE-IN  loop ;

: LLA-STAGE-RED ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LRED-ANALYZE
   LRED-NIN@ LLA-NIN ! LRED-ELEMS LLA-ELEMS ! LRED-OUT-NODE@ LLA-OUT-NODE!
   LRED-NIN@ 0 ?do  i  i LRED-IN-REF@  LLA-STAGE-IN  loop ;

: LLA-STAGE-MM ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LMM-ANALYZE
   LMM-NIN@ LLA-NIN ! LMM-ELEMS LLA-ELEMS ! LMM-OUT-NODE@ LLA-OUT-NODE!
   LMM-NIN@ 0 ?do  i  i LMM-IN-REF@  LLA-STAGE-IN  loop
   LMM-M@ LLA-PM !  LMM-N@ LLA-PN !  LMM-K@ LLA-PK ! ;

\ movement copy region (maki/lower-move.f): 1-2 buffer operands are already input slots
: LLA-STAGE-MV ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LMV-ANALYZE
   LMV-NIN@ LLA-NIN ! LMV-ELEMS LLA-ELEMS ! LMV-OUT-NODE@ LLA-OUT-NODE!
   LMV-NIN@ 0 ?do
      i LMV-IN-REF@ i LLA-IN-REF!
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

: LLA-EXEC-MM ( CAD-KIND:region n n -- ) {: rid:CAD-KIND:region gx:n gy:n :}
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

: LLA-EXEC-MV ( CAD-KIND:region n -- ) {: rid:CAD-KIND:region grid:n :}
   LLA-ELEMS @ LLA-NCAP > if E-LLA-CAP throw then
   LLA-NIN @ 0 ?do  i LLA-IN-ELEMS-I LLA-NCAP > if E-LLA-CAP throw then  loop
   LLA-ELEMS @ 4 * {: obytes:n :}
   LLA-NIN @ 0 ?do i LLA-PACK-INPUT loop
   rid LLA-FNAME  LLA-SETUP  obytes LLA-ALLOC-UPLOAD
   grid obytes LLA-LAUNCH-MV
   LLA-RELEASE ;

\ ---- grid + u32 kernel-param computation per shape (staging already ran) --------
\ each sets the shape's u32 param (LLA-NVAR = p_n or p_k; matmul uses LLA-PM/PN/PK) and
\ returns the launch grid; shared by the single-region RUN words and the whole-model exec.
: LLA-GRID-EW ( -- n )
   LLA-ELEMS @ {: n:n :}
   n PTX-LAUNCH-POSITIVE  LLA-BLOCK PTX-BLOCK-CHECK
   n LLA-NVAR !
   n LLA-BLOCK + 1 - LLA-BLOCK / ;
\ typed rows/cols -> row-launch grid + u32 param: the raw projection is confined
\ HERE (grid=rows, p_k=cols), so a rows/cols swap at a call site is a checker
\ reject; the runtime PTX-ROW-LAUNCH-CHECK stays behind the seam (defense in depth).
: LLA-ROW-GRID ( CAD-KIND:rows CAD-KIND:cols -- n )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   rows ROWS-RAW cols COLS-RAW LLA-BLOCK PTX-ROW-LAUNCH-CHECK
   cols COLS-RAW LLA-NVAR !
   rows ROWS-RAW ;
: LLA-GRID-RED ( -- n )
   LRED-ROWS@ LRED-COLS@ LLA-ROW-GRID ;
\ grid tiles the OUTPUT by the emitted kernel's tile edge (LMM-OUT-TILE@ = 64 register-blocked
\ / 16 naive); the block shape stays 16x16 = 256 threads for both (LLA-MM-TILE). LLA-STAGE-MM
\ (LMM-ANALYZE) fixed the blocked/naive choice before this runs, so the divisor matches the emit.
: LLA-GRID-MM ( -- n n )
   LLA-PM @ PTX-LAUNCH-POSITIVE  LLA-PN @ PTX-LAUNCH-POSITIVE  LLA-PK @ PTX-LAUNCH-POSITIVE
   LMM-OUT-TILE@ {: tile:n :}                               \ 64 (register-blocked) or 16 (naive)
   LLA-PN @ tile + 1 - tile /                               \ ceil(N/tile)
   LLA-PM @ tile + 1 - tile / ;                             \ ceil(M/tile)
: LLA-GRID-MV ( -- n )
   LLA-ELEMS @ {: n:n :}
   n PTX-LAUNCH-POSITIVE  LLA-BLOCK PTX-BLOCK-CHECK
   n LLA-NVAR !
   n LLA-BLOCK + 1 - LLA-BLOCK / ;

public

\ ---- region class dispatch (a region never mixes contraction/reduction; the golden reuses
\ these). A materialized-movement region is a single movement node with MIR-MAT@ set. -------
: LLA-REGION-MATMUL? ( CAD-KIND:region -- bool )  FP-REGION-CLASSMIX  1 CLASS-MATMUL     lshift  and  0= 0= ;
: LLA-REGION-REDUCE? ( CAD-KIND:region -- bool )  FP-REGION-CLASSMIX  1 CLASS-ROW-REDUCE lshift  and  0= 0= ;
: LLA-REGION-MOVE? ( CAD-KIND:region -- bool ) {: rid:CAD-KIND:region :}
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-RID@ rid FP-RGN= node MIR-MOVE? and node MIR-MAT@ and if unloop true exit then
   loop false ;

private

\ ======================= whole-model device execution (slice 5) =================
\ LOWER-MODEL-RUN executes every region of the current forward IR on device in topo
\ (materialized-node) order. Each region's materialized output stays in a context-scoped
\ device buffer (MDL-BUF, keyed by node index); a downstream region whose input names that
\ producer node BINDS the buffer instead of uploading, so cross-region compute and a
\ materialized movement copy reading a producer buffer run without the slots-only cap.

128 constant MDL-CAP                              \ mirrors MIR-CAP / fusion-plan FP-CAP
create MDL-CUBINS    MDL-CAP FS-PATH-CAP * allot   \ per-region REGION_<rid> cubin path text
create MDL-CUBIN-LEN MDL-CAP cells allot            \ per-region cubin path length (0 = unset)
create MDL-BUF       MDL-CAP cells allot            \ per-node device buffer (devptr int; 0 = none)
create MDL-OWN       LLA-MAX-IN cells allot         \ per-input "uploaded (owned)" flag, current region
variable MDL-NEW  variable MDL-NRED  variable MDL-NMM  variable MDL-NMV  variable MDL-NR
1 LAYOUT-BUFFER MDL-PROBE-RID CAD-KIND:region      \ typed region cell carried into the lowerability probe (catch wants a ( -- ) body)

\ region-indexed cubin tables: RGN>RAW is the one owner-file table-index boundary
: MDL-CUBIN$ ( CAD-KIND:region -- ptr u8 n ) {: rid:CAD-KIND:region :}
   rid RGN>RAW {: r:n :}
   r cells MDL-CUBIN-LEN + @ {: u:n :}
   u 0= if E-MDL-CUBIN throw then
   MDL-CUBINS r FS-PATH-CAP * +  u ;
: MDL-DEVPTR@ ( CAD-KIND:node-id -- n ) NODE>RAW cells MDL-BUF + @ ;
: MDL-DEVPTR! ( n CAD-KIND:node-id -- ) {: dp:n node:CAD-KIND:node-id :}
   dp node NODE>RAW cells MDL-BUF + ! ;
: MDL-BUF-RESET ( -- )  MDL-CAP 0 ?do  0 i cells MDL-BUF + !  loop ;

\ per-input buffer provision: upload a model slot, or bind an already-produced node buffer
: MDL-UP-SLOT ( n -- ) {: i:n :}
   i LLA-PACK-INPUT                                 \ pack slot synthetic into LLA-HIN[i]
   i LLA-IN-ELEMS-I 4 * {: ib:n :}
   i LLA-DBUF-I ib >LEN CUDA:CUMEMALLOC CUDA:RC0
   i LLA-DBUF-I @ >CUDA-DEVPTR  i LLA-HIN-I  ib >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   1 i cells MDL-OWN + ! ;
: MDL-BIND-NODE ( n -- ) {: i:n :}
   i LLA-IN-REF@ MIR-REF-NODE {: node:CAD-KIND:node-id :}
   node MDL-DEVPTR@ {: dp:n :}
   dp 0= if E-MDL-UNRESOLVED throw then             \ topo order must have produced it
   dp i LLA-DBUF-I !                                \ reuse the producer's device buffer
   0 i cells MDL-OWN + ! ;
: MDL-PROVIDE-INPUTS ( -- )
   LLA-NIN @ 0 ?do
      i LLA-IN-REF@ MIR-REF-INPUT? if i MDL-UP-SLOT else i MDL-BIND-NODE then
   loop ;
: MDL-ALLOC-OUT ( -- )                             \ region output buffer; record it under its node
   LLA-ELEMS @ 4 * {: ob:n :}
   LLA-NIN @ LLA-DBUF-I ob >LEN CUDA:CUMEMALLOC CUDA:RC0
   LLA-NIN @ LLA-DBUF-I @ LLA-STAGED-NODE@ MDL-DEVPTR! ;
: MDL-FREE-OWNED ( -- )                            \ free only the per-region uploaded inputs
   LLA-NIN @ 0 ?do
      i cells MDL-OWN + @ if  i LLA-DBUF-I @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0  then
   loop ;
: MDL-CHECK-CAPS ( -- )
   LLA-ELEMS @ LLA-NCAP > if E-LLA-CAP throw then
   LLA-NIN @ 0 ?do  i LLA-IN-ELEMS-I LLA-NCAP > if E-LLA-CAP throw then  loop ;

: MDL-LAUNCH-1D ( n -- ) {: grid:n :}
   LLA-FUNC @ >CUDA-FN grid 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0 ;
: MDL-LAUNCH-2D ( n n -- ) {: gx:n gy:n :}
   LLA-FUNC @ >CUDA-FN gx gy CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0 ;

\ stage a region by class (analysis + per-input operand resolution into launch staging)
: MDL-STAGE ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LLA-REGION-MOVE?   if rid LLA-STAGE-MV  exit then
   rid LLA-REGION-MATMUL? if rid LLA-STAGE-MM  exit then
   rid LLA-REGION-REDUCE? if rid LLA-STAGE-RED exit then
   rid LLA-STAGE-EW ;

\ grid + param bind + launch by class (LLA-DBUF already holds inputs + output devptrs)
: MDL-DISPATCH ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LLA-REGION-MOVE? if
      LLA-GRID-MV {: g0:n :}  LLA-BIND-PARAMS-MV  g0 MDL-LAUNCH-1D  exit then
   rid LLA-REGION-MATMUL? if
      LLA-GRID-MM {: gx:n gy:n :}  LLA-BIND-PARAMS-MM  gx gy MDL-LAUNCH-2D  exit then
   rid LLA-REGION-REDUCE? if
      LLA-GRID-RED {: g1:n :}  LLA-BIND-PARAMS  g1 MDL-LAUNCH-1D  exit then
   LLA-GRID-EW {: g2:n :}  LLA-BIND-PARAMS  g2 MDL-LAUNCH-1D ;

: MDL-EXEC-REGION ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid MDL-STAGE
   MDL-CHECK-CAPS
   rid LLA-FNAME                                   \ LLA-FN = REGION_<rid>
   rid MDL-CUBIN$ LLA-MOD-OPEN
   MDL-PROVIDE-INPUTS
   MDL-ALLOC-OUT
   rid MDL-DISPATCH
   MDL-FREE-OWNED
   LLA-MOD-CLOSE ;

\ read the final model output node's device buffer back into LLA-HOUT (guarded), and point
\ the golden accessors (LLA-OUT-NODE / LLA-ELEMS) at it so LG-COMPARE-LIN reads the model output.
: MDL-READBACK ( -- )
   MIR-N@ 1- MIR-NODE-ID {: out:CAD-KIND:node-id :}
   out MIR-ROWS@ out MIR-COLS@ SHAPE-ELEMS DIM-RAW {: e:n :}
   out MDL-DEVPTR@ {: dp:n :}
   dp 0= if E-MDL-NOOUT throw then
   e 4 * {: ob:n :}
   LLA-HRB ob PTXSENT:FILL
   LLA-HRB  dp >CUDA-DEVPTR  ob >LEN CUDA:CUMEMCPYDTOH CUDA:RC0
   e 0 ?do  LLA-HRB i 4 * + SF-LD PTXSENT:GUARD F32>F64  LLA-HOUT i T-SET  loop
   out LLA-OUT-NODE! e LLA-ELEMS ! ;
: MDL-FREE-ALL ( -- )
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :} node MDL-DEVPTR@ {: dp:n :}
      dp 0= 0= if dp >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0 0 node MDL-DEVPTR! then
   loop ;

public

\ ---- per-region cubin registry (device tool assembles REGION_<rid>, one cubin per region) --
: MDL-CUBIN! ( ptr u8 n CAD-KIND:region -- ) {: a:ptr u:n rid:CAD-KIND:region :}
   rid RGN>RAW {: r:n :}                              \ table-index boundary (RGN>RAW)
   r 0 < r MDL-CAP >= or if E-MDL-CUBIN throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a  MDL-CUBINS r FS-PATH-CAP * +  u BYTE-COPY
   u r cells MDL-CUBIN-LEN + ! ;
: MDL-CUBINS-RESET ( -- )  MDL-CAP 0 ?do  0 i cells MDL-CUBIN-LEN + !  loop ;

\ every materialized-node region has a registered cubin (the device golden gate needs assembled
\ kernels; without them the gate must fall back to the host leg rather than throw E-MDL-CUBIN).
: MDL-CUBINS-READY? ( -- bool )
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node MIR-MAT@ if
         node FP-RID@ RGN>RAW cells MDL-CUBIN-LEN + @   \ table-index boundary (RGN>RAW)
         0= if false unloop exit then
      then
   loop  true ;

\ ---- per-class region tally over the materialized nodes (tolerance composition inputs) ----
: MDL-COUNT-REGIONS ( -- )
   0 MDL-NEW !  0 MDL-NRED !  0 MDL-NMM !  0 MDL-NMV !  0 MDL-NR !
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node MIR-MAT@ if
         node FP-RID@ {: rid:CAD-KIND:region :}
         MDL-NR @ 1+ MDL-NR !
         rid LLA-REGION-MOVE? if     MDL-NMV  @ 1+ MDL-NMV !
         else rid LLA-REGION-MATMUL? if MDL-NMM @ 1+ MDL-NMM !
         else rid LLA-REGION-REDUCE? if MDL-NRED @ 1+ MDL-NRED !
         else MDL-NEW @ 1+ MDL-NEW ! then then then
      then
   loop ;
: MDL-N-EW@      ( -- n )  MDL-NEW @ ;
: MDL-N-RED@     ( -- n )  MDL-NRED @ ;
: MDL-N-MM@      ( -- n )  MDL-NMM @ ;
: MDL-N-MV@      ( -- n )  MDL-NMV @ ;
: MDL-N-REGIONS@ ( -- n )  MDL-NR @ ;

\ device-lowerability probe: every materialized region must analyze (its class staging must
\ not fail closed) AND every op must be host-executable (the golden runs the host reference).
\ The analyzers' rejection throws ARE the "not device-lowerable" signal, so they are probed
\ under catch here (a capability check, not error masking); a real device error is not caught.
: MDL-LOWERABLE? ( -- bool )
   GA-SUPPORTED? 0= if false exit then
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node MIR-MAT@ if
         node FP-RID@ 0 MDL-PROBE-RID !          \ typed region round-trips through the cell (checker-enforced)
         [: 0 MDL-PROBE-RID @ MDL-STAGE ;] catch 0<> if  false unloop exit  then
      then
   loop  true ;

\ LOWER-MODEL-RUN executes the whole forward IR on device, region by region. Requires the
\ fusion plan built (FP-BUILD), synthetic inputs bound (GA-BIND-SYNTH), and each region's
\ cubin registered (MDL-CUBIN!). Leaves LLA-HOUT = the final model output (f64), LLA-OUT-NODE
\ / LLA-ELEMS pointing at it for the golden compare.
: LOWER-MODEL-RUN ( -- )
   LLA-CTX-OPEN
   MDL-BUF-RESET
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node MIR-MAT@ if node FP-RID@ MDL-EXEC-REGION then
   loop
   MDL-READBACK
   MDL-FREE-ALL
   LLA-CTX-CLOSE ;

public

\ LLA-RUN analyzes elementwise region rid, uploads its synthetic inputs, launches the
\ REGION_<rid> flat kernel (grid = ceil(N/256)) from LLA-CUBIN$, and unpacks the device
\ output into LLA-HOUT. GA-BIND-SYNTH must have run so GA-IN-PTR holds the inputs.
: LLA-RUN ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LLA-STAGE-EW  LLA-GRID-EW {: grid:n :}  rid grid LLA-EXEC ;

\ LRED-RUN analyzes row-reduce region rid and launches its REGION_<rid> block-per-row
\ kernel (grid = rows, p_k = cols). Same synthetic-input upload + guarded readback.
: LRED-RUN ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LLA-STAGE-RED  LLA-GRID-RED {: grid:n :}  rid grid LLA-EXEC ;

\ LMM-RUN analyzes matmul/linear region rid and launches its REGION_<rid> tiled-GEMM
\ kernel (2D grid ceil(N/16) x ceil(M/16), block 16x16, params M/N/K). Contraction
\ operands (A, B, [bias]) upload at their own sizes; same guarded readback into LLA-HOUT.
: LMM-RUN ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LLA-STAGE-MM  LLA-GRID-MM {: gx:n gy:n :}  rid gx gy LLA-EXEC-MM ;

\ LMV-RUN analyzes a materialized movement region (maki/lower-move.f) and launches its
\ REGION_<rid> copy kernel (grid = ceil(N/256), one elem/thread, p_a/p_b/p_n u32). Buffer
\ operands upload at their own sizes (concat A/B, gather source+index differ). Same guarded
\ readback into LLA-HOUT.
: LMV-RUN ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LLA-STAGE-MV  LLA-GRID-MV {: grid:n :}  rid grid LLA-EXEC-MV ;

\ device output element (f64 = the widened device f32) after LLA-RUN / LRED-RUN / LMM-RUN
: LLA-OUT@ ( n -- r )  LLA-HOUT swap T-GET ;

;package
