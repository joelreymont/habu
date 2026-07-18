\ tile-pipe-neg-test.f - committed negative regressions for the pipelined-GEMM
\ typed obligations (lib/ptx/tile-pipe.f).
\
\ Seven source-equivalent bodies, each rejected fail-closed by the checker with a
\ located diagnostic. These pin the stage-1 soundness rules the CURRENT type
\ system states nominally (parallel to tile-v4a-neg-test.f); the dynamic pipeline
\ protocol negatives (read-before-wait, missing commit, double-wait, parity
\ ALTERNATION across iterations) belong to the checker capability
\ habu-checker-cp-async-6ba788a5 and land with it.
\
\   N1 cross-parity operands - RB-FMA over fragments of two different buffer
\                              parities: operands of one FMA step must come from
\                              THE SAME staged buffer.
\   N2 strided v4 base       - the strided A slice (4B-aligned rows) into the
\                              vectorized B-FRAG.V4 load: only the contiguous
\                              16B-proven mmbslice is a legal v4 base.
\   N3 unproven v4 base      - a plain shared span (tile-smem STAGE product, no
\                              alignment proof) into B-FRAG.V4.
\   N4 cross-layout operands - an mmafrag from a DIFFERENT blocked layout atom
\                              into RB-FMA: geometry rides the types, layouts
\                              never mix.
\   N5 naive store           - the register-blocked accumulator through the
\                              scalar STORE: mmracc is not a tile<>; only
\                              PIPE-STORE finalizes it.
\   N6 naive shared load     - the pipelined stage through tile-smem's scalar
\                              SLOAD: mmstage is not a shared span.
\   N7 acc-dropping body     - a PIPE-LOOP body that drops the accumulator and
\                              returns the stage: the loop body must be
\                              accumulator-preserving. This is ALSO the shared-tile
\                              LIFETIME-ESCAPE pin (dot habu-v2-checked-async): the
\                              staged shared tile (mmstage) is minted fresh per
\                              iteration INSIDE PIPE-LOOP and the body's declared
\                              effect [ mmstage mmracc -- mmracc ] forbids it in the
\                              output, so a staged shared value cannot escape its
\                              pipeline scope - reading it is bounded below by
\                              READ-STAGE (needs a block-visible cpp-ready<p> slot)
\                              and above by this consume-only body contract.

require lib/ptx/neg-test-lib.f
require lib/ptx/cg-matmul.f
require lib/ptx/tile-pipe.f

: TPN-MAIN ( -- )
   T-RESET
   256 %BLOCK

   s" BAD-TP-PARITY ( mmracc<f32,block-256,geom-mt4x4,mask-live> mmafrag<f32,block-256,geom-as64x32-bs32x64,mask-live,parity-a> mmbfrag<f32,block-256,geom-as64x32-bs32x64,mask-live,parity-b> -- mmracc<f32,block-256,geom-mt4x4,mask-live> ) RB-FMA"
   s" parity-a" s" tile-pipe cross-parity negative" PTXN-REJECTS
   s" NEG: RB-FMA over two buffer parities rejected (same-stage operand rule)" type cr

   s" BAD-TP-AV4 ( mmaslice<f32,block-256,geom-as64x32-bs32x64,mask-live,parity-a> -- ) {: s :} s 0 B-FRAG.V4 drop"
   s" mmaslice" s" tile-pipe strided-v4-base negative" PTXN-REJECTS
   s" NEG: strided A slice into B-FRAG.V4 rejected (16B contiguous obligation)" type cr

   s" BAD-TP-SPAN ( span<space-shared,f32,extent-n> -- ) {: s :} s 0 B-FRAG.V4 drop"
   s" space-shared" s" tile-pipe unproven-v4-base negative" PTXN-REJECTS
   s" NEG: plain shared span into B-FRAG.V4 rejected (alignment-proven mmbslice only)" type cr

   s" BAD-TP-LAYOUT ( mmracc<f32,block-256,geom-mt4x4,mask-live> mmafrag<f32,block-256,geom-other,mask-live,parity-a> mmbfrag<f32,block-256,geom-as64x32-bs32x64,mask-live,parity-a> -- mmracc<f32,block-256,geom-mt4x4,mask-live> ) RB-FMA"
   s" geom-other" s" tile-pipe cross-layout negative" PTXN-REJECTS
   s" NEG: fragment from a different blocked layout rejected (layout atom pinned)" type cr

   s" BAD-TP-STORE ( mmracc<f32,block-256,geom-mt4x4,mask-live> span<space-global,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- ) STORE"
   s" mmracc" s" tile-pipe naive-store negative" PTXN-REJECTS
   s" NEG: register-blocked accumulator through scalar STORE rejected (mmracc != tile)" type cr

   s" BAD-TP-SLOAD ( mmstage<f32,block-256,geom-as64x32-bs32x64,mask-live,parity-a> coopctx<block-256,extent-n,mask-live> -- ) SLOAD drop"
   s" mmstage" s" tile-pipe naive-sload negative" PTXN-REJECTS
   s" NEG: pipelined stage through scalar SLOAD rejected (mmstage != shared span)" type cr

   s" BAD-TP-DROP ( mmctx<extent-m,extent-k,extent-n> mmracc<f32,block-256,geom-mt4x4,mask-live> -- mmctx<extent-m,extent-k,extent-n> mmracc<f32,block-256,geom-mt4x4,mask-live> ) [: drop ;] PIPE-LOOP"
   s" mmstage" s" tile-pipe acc-dropping-body negative" PTXN-REJECTS
   s" NEG: accumulator-dropping PIPE-LOOP body rejected (acc-preserving contract)" type cr

   T-REPORT ;

TPN-MAIN
