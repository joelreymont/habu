\ mma-ablate.f - DCE-safe timing decomposition of the wider-M TF32 mma.sync GEMM (lib/ptx/cg-mma.f).
\
\ dot habu-mma-amortize-the-dd182428. Productizes the attribution-lane method (nsys GPU-metrics is
\ UNSUPPORTED on the Orin iGPU, so DCE-safe ablated kernel VARIANTS timed same-session are the only
\ way to attribute per-phase cost). Emits the wide config (MFRAGS=2 BK=32 pad=8 stages=2 dyn ldmatrix,
\ 128x64) at each MMA-ABLATE mode and times it; every mode keeps ALL mma + stores live (ptxas cannot
\ DCE them) but drops part of the feed, so the same-session TIME delta isolates each cost:
\   0 full          - the real kernel (correct; the parity number)
\   1 quarter-B     - B loaded 1/4 (once per K-substep, reused across 4 n-tiles): the CEILING proxy
\   2 half-B        - B loaded 1/2 (n-tiles 0,2): the B-feed slope midpoint
\   3 single-mma    - only M-frag 0's mma per n-tile: isolates the 2nd M-frag mma-issue cost
\ GFLOP/s here is a PROXY (work is the same 2*s^3, results in modes 1-3 are WRONG on purpose) - it
\ expresses "the rate the full kernel WOULD hit at this phase's time". Correctness is owned by
\ tools/ptx/mma-gemm-check.f; real perf by tools/ptx/gemm-bench.f. Device-only: off the Orin (no
\ libcuda) MA-ALL SKIPS so this file still check-loads. Run pinned at 918 MHz on the device.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/cg-matmul.f
require lib/ptx/cg-mma.f
require lib/ptx/toolchain.f
require maki/eval/active-target.f
require tools/ptx/bench.f

package MMAABLATE

$3F800000 constant MA-ONE                \ 1.0f bit pattern (A/B fill)
create MA-QO $1000 allot  create MA-QE $1000 allot
variable MA-DA  variable MA-DB  variable MA-DC
variable MA-NV                           \ M=N=K (square) kernel param
variable MA-S                            \ square edge held for the owning scope body (local lifted to var)
variable MA-NS-TMP                       \ BENCH-GPU-NS result held across the owning scope's frame exit

: MA-INT. ( n -- )  SB-RESET FMT:SB-INT SB$ type ;

: MA-ASSEMBLE ( -- )                     \ captured PTX -> ptxas cubin (die on rc<>0)
   ATGT:LABEL$ PTXTC:TC-ARCH!            \ assembler arch from the probed active target (else PTXTC:ASSEMBLE fails closed on the GB10)
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   MA-QO $1000 >LEN MA-QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT {: rc:n :}
   rc 0= 0= if s" mma-ablate: ptxas failed" 1 die then ;

: MA-ALLOC ( n -- ) {: s:n :}            \ A=B=1.0, C=0 for an s x s GEMM
   s s * {: e:n :}
   e 4 * MA-DA PTXBENCH:DEVICE-ALLOC  MA-DA PTXBENCH:OWN-DEV
   e 4 * MA-DB PTXBENCH:DEVICE-ALLOC  MA-DB PTXBENCH:OWN-DEV
   e 4 * MA-DC PTXBENCH:DEVICE-ALLOC  MA-DC PTXBENCH:OWN-DEV
   MA-DA @ MA-ONE e PTXBENCH:DEVICE-MEMSET32
   MA-DB @ MA-ONE e PTXBENCH:DEVICE-MEMSET32
   MA-DC @ 0     e PTXBENCH:DEVICE-MEMSET32 ;

: MA-PARAMS ( n -- ) {: s:n :}           \ block 16x16; gridX=s/64 (BN), gridY=s/BROWS (wide M)
   s MA-NV !
   16 PTXBENCH:BLOCK!  16 PTXBENCH:BLOCKY!
   s 64 / PTXBENCH:GRID!  s MMA-BROWS / PTXBENCH:GRIDY!
   36 PTXBENCH:PARAM-BYTES!
   MMA-DYNSMEM @ if MMA-SMEM PTXBENCH:SHARED! else 0 PTXBENCH:SHARED! then
   PTXBENCH:PREPARE-LAUNCH
   0  MA-DA PTXBENCH:PARAM-PTR!
   8  MA-DB PTXBENCH:PARAM-PTR!
   16 MA-DC PTXBENCH:PARAM-PTR!
   24 MA-NV PTXBENCH:PARAM-U32!  28 MA-NV PTXBENCH:PARAM-U32!  32 MA-NV PTXBENCH:PARAM-U32! ;

\ the wide config under study (MFRAGS/stages/dyn selectable; dot habu-mma-wave-2 adds the MFRAGS=4 leg;
\ dot habu-mma-wave-3 adds the B-side ldmatrix knobs BLDM/BPAD to re-attribute the residual B-feed).
variable MA-CFG-MFRAGS  variable MA-CFG-STAGES  variable MA-CFG-DYN
variable MA-CFG-BLDM    variable MA-CFG-BPAD
2 MA-CFG-MFRAGS !  2 MA-CFG-STAGES !  1 MA-CFG-DYN !  0 MA-CFG-BLDM !  0 MA-CFG-BPAD !
: MA-WIDE-KNOBS ( -- )                   \ apply the config under study (mode reset per ablate leg)
   MA-CFG-MFRAGS @ MMA-MFRAGS !  32 MMA-BK !  8 MMA-PAD !
   MA-CFG-STAGES @ MMA-STAGES !  MA-CFG-DYN @ MMA-DYNSMEM !  2 MMA-LMODE !
   MA-CFG-BLDM @ MMA-BLDM !  MA-CFG-BPAD @ MMA-BPAD ! ;
: MA-CFG! ( n n n -- ) {: mfrags:n stages:n dyn:n :}   \ select the scalar-B config under study (BLDM off)
   mfrags MA-CFG-MFRAGS !  stages MA-CFG-STAGES !  dyn MA-CFG-DYN !  0 MA-CFG-BLDM !  0 MA-CFG-BPAD ! ;
: MA-CFG-B! ( n n n n -- ) {: mfrags:n stages:n dyn:n bpad:n :}   \ select a B-side-ldmatrix config under study
   mfrags MA-CFG-MFRAGS !  stages MA-CFG-STAGES !  dyn MA-CFG-DYN !  1 MA-CFG-BLDM !  bpad MA-CFG-BPAD ! ;
: MA-RESTORE ( -- )                      \ back to the committed defaults
   1 MMA-MFRAGS !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !  0 MMA-ABLATE !
   0 MMA-BLDM !  0 MMA-BPAD ! ;

: MA-LABEL ( n -- ) {: mode:n :}
   mode 0= if s" full        " type then
   mode 1 = if s" quarter-B   " type then
   mode 2 = if s" half-B      " type then
   mode 3 = if s" single-mma  " type then ;

\ emit the wide kernel at ablate `mode`, assemble, load, time at shape s (it iters); print ns + proxy GFLOP/s
: MA-RUN ( n n n -- n ) {: mode:n s:n it:n :}
   MA-WIDE-KNOBS  mode MMA-ABLATE !
   s" habu-mma-ablate" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   MA-ASSEMBLE
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" MMM" PTXBENCH:KERNEL!  s" MMM" PTXBENCH:LABEL!
   it PTXBENCH:ITERS!
   s MA-S !
   [: PTXBENCH:OPEN  PTXBENCH:OWN-CTX  PTXBENCH:LOAD  PTXBENCH:OWN-MOD   \ scope owns ctx+module+buffers; unwinds on return or throw
      MA-S @ MA-ALLOC  MA-S @ MA-PARAMS
      PTXBENCH:BENCH-GPU-NS MA-NS-TMP ! ;] CUDA-SCOPE:SCOPE
   PTXTC:CLEAN
   s" ablate=" type mode MA-LABEL  s"  " type s MA-INT. s" ^3  gpu_ns=" type MA-NS-TMP @ MA-INT.
   s"  proxy_GFLOP/s_x1000=" type  s s * s * 2 * it *  1000 * MA-NS-TMP @ /  MA-INT. cr
   MA-NS-TMP @ ;

: MA-SHAPE ( n -- ) {: s:n :}            \ full/quarter/half/single at shape s; print the decomposition
   s 2048 = if 30 else 80 then {: it:n :}
   s" == wider-M ablation decomposition, MFRAGS=" type MA-CFG-MFRAGS @ MA-INT. s"  stages=" type
   MA-CFG-STAGES @ MA-INT.  s"  dyn=" type MA-CFG-DYN @ MA-INT.  s"  bldm=" type MA-CFG-BLDM @ MA-INT.
   s"  bpad=" type MA-CFG-BPAD @ MA-INT.  s"  " type s MA-INT. s" ^3 (918 MHz pin) ==" type cr
   0 s it MA-RUN {: full:n :}
   1 s it MA-RUN {: qb:n :}
   2 s it MA-RUN {: hb:n :}
   3 s it MA-RUN {: sm:n :}
   MA-RESTORE
   s" -- attribution: full=" type full MA-INT. s" ns  ceiling(quarter-B)=" type qb MA-INT.
   s"  proxy_ceiling_GFLOP/s_x1000=" type  s s * s * 2 * it *  1000 * qb /  MA-INT. cr
   s" -- B-feed cost (full - quarter-B, ~3/4 of B loads) = " type full qb - MA-INT. s"  ns" type cr
   s" -- 2nd-M-frag mma-issue cost (full - single-mma) = " type full sm - MA-INT. s"  ns" type cr ;

public
: MA-ALL ( -- )
   CUDA:OPEN? 0= if s" mma-ablate: libcuda unavailable -> SKIPPED (off-device)" type cr MA-RESTORE exit then
   s" == MMM-WIDE ldmatrix tiles: DCE-safe per-phase cost decomposition (918 MHz pin) ==" type cr
   2 2 1 MA-CFG!                          \ MFRAGS=2 parity config (128x64, stages=2 dyn) - reference attribution
   2048 MA-SHAPE
   4 1 0 MA-CFG!                          \ MFRAGS=4 winner (256x64, stages=1 static) - dot habu-mma-wave-2
   2048 MA-SHAPE
   4 1 1 4 MA-CFG-B!                       \ MFRAGS=4 B-side ldmatrix (256x64, stages=1 dyn, bpad=4) - dot habu-mma-wave-3
   2048 MA-SHAPE
   MA-RESTORE ;
;package

MMAABLATE:MA-ALL
