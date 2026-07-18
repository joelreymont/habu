\ maki/lower/model-mlp-device-test.f - the committed small-model e2e proof (GAP #6,
\ dot habu-small-model-end): the dot's named MLP shape LINEAR RELU LINEAR lowered through the
\ REAL maki pipeline (OPTIMIZE -> fusion regions -> per-region cubins) and run END-TO-END on the
\ Orin, with the final output matching the CPU/host executor within the composed tolerance.
\
\ The model is the canonical two-layer MLP: LINEAR RELU LINEAR (x:4x8, w1:8x16, b1:1x16, w2:16x8,
\ b2:1x8). It fuses into 2 MATMUL regions - region 0 = matmul + RELU epilogue (the epilogue-fusion
\ lever), region 1 = matmul reading region 0's device buffer (cross-region f32 hand-off). The
\ whole-model golden (maki/lower/golden.f LOWER-MODEL-GOLDEN via LOWER-MODEL-RUN) runs both regions
\ on device region-by-region and compares the FINAL output against the host executor under the
\ composed tolerance (summed per-region matmul rtol/atol - the lower-golden discipline). Each device
\ readback is sentinel-poisoned + GUARDed, so a dropped/partial copy fails closed rather than passing
\ on stale data.
\
\ SCOPE - forward/eval e2e, not training. This dot proves the FORWARD (eval) pass end-to-end on
\ device vs CPU. On-device TRAINING (a device backward + optimizer step) needs the autograd/adjoint
\ regions lowered onto device kernels, which LOWER-MODEL-RUN does not yet do (its region classes are
\ matmul / row-reduce / elementwise / movement - the forward ops); that autograd-on-device half is
\ tracked by dot habu-...-ee4d918b. The maki CPU side already trains (maki/train-test.f,
\ maki/from-scratch-model-test.f); this is the device leg of the eval/forward pass.
\
\ CORRUPTION SENSITIVITY - that a wrong pipeline is REJECTED (not silently passed) is proven for the
\ shared whole-model golden comparator (LG-COMPARE-LIN) by maki/ablate-golden-device-test.f: seeded
\ PTX-mutation (wrong index / transposed operand), stale-cubin, and dropped-copy-back faults are each
\ caught as V-FAIL or a fail-closed throw on the Orin. A magnitude-independent PTX-mutation of THIS
\ MLP's region-0 matmul kernel likewise drives the whole-model golden to V-FAIL (demonstrated in a
\ temp copy on the Orin; not committed here - the committed proof stays the clean PASS).
\
\ RESIDUAL - no attention region class. An attention block (one head) is NOT lowerable as a whole-
\ model region here: LOWER-MODEL-RUN dispatches only matmul / row-reduce / elementwise / movement
\ region classes (maki/lower/launch.f LLA-REGION-*). The fused attention kernel (lib/ptx/cg-attention.f
\ ATTN) exists and is device-gold'd standalone (tools/ptx/device-gold.f), but it is not wired as a
\ maki region class, so a MODEL: ... ATTENTION ... cannot lower through this whole-model path yet.
\ The missing piece is an attention region class in the fusion planner + a LLA attention launch shape.
\
\ Off the Orin (no libcuda) the device leg is SKIPPED and the host build still loads. Not part of
\ the maki gate (maki/test.f) - it needs the CUDA toolkit + a device. Run on the Orin: scp to
\ zed:Work/habu then `bin/hb --load maki/lower/model-mlp-device-test.f`.

require maki/lower/model-device.f

package MAKI
LMDM-BEGIN

\ ============ the small model: LINEAR RELU LINEAR (2 matmul regions, relu epilogue fusion) ======
\ region 0 = linear + relu (matmul epilogue), region 1 = linear (reads region 0's device buffer).
\ The device carries f32 across the region boundary; composed tol = 2*mm rtol/atol.
s" == MLP LINEAR RELU LINEAR 4x8 (whole model, relu epilogue fusion) ==" type cr
MODEL: MLPRELU ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 -- y ) LINEAR RELU LINEAR ;  FP-BUILD
s" MODEL: MLPRELU ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 -- y ) LINEAR RELU LINEAR ;" LMDM-GOLD

LMDM-END
;package
