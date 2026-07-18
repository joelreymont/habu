\ maki/lower/model-device-test.f - the Orin device leg for the WHOLE-MODEL run (slice 5).
\
\ The whole-model device-golden HARNESS (spawn per-region emit -> ptxas -> register cubins ->
\ LOWER-MODEL-GOLDEN -> per-element evidence, plus the cad.f OPTIMIZE/PROMOTE gate and the
\ off-device SKIP scaffolding) lives in maki/lower/model-device.f and is shared with
\ maki/lower/model-mlp-device-test.f. THIS file only stages the FFN-class fixtures and drives them.
\
\ Two legs: (A) the FFN-class chain LINEAR GELU LINEAR RMSNORM - 3 regions (matmul+gelu epilogue,
\ matmul, row-reduce) where region 1 reads region 0's device buffer and region 2 reads region 1's;
\ (B) cross-region MOVEMENT GELU CONCAT - region 1 (a materialized copy) reads region 0's (gelu)
\ device buffer. Then the cad.f gate path on the FFN model: OPTIMIZE records golden device-pass and
\ PROMOTE writes an evidence row with golden=device-pass:f32 (the default licensed precision).
\
\ Off the Orin (no libcuda) the device legs are SKIPPED and the host build still loads. Not part of
\ the maki gate (maki/test.f) - it needs the CUDA toolkit + a device. Run on the Orin: scp to
\ zed:Work/habu then `bin/hb --load maki/lower/model-device-test.f`.

require maki/lower/model-device.f

package MAKI
LMDM-BEGIN

\ ============ (A) FFN-class multi-region model: LINEAR GELU LINEAR RMSNORM =====================
\ region 0 = linear+gelu (matmul), region 1 = linear (reads region 0), region 2 = rmsnorm (reads
\ region 1). The device carries f32 across each region boundary; composed tol = 2*mm + 1*red rtol.
s" == FFN LINEAR GELU LINEAR RMSNORM 4x8 (cross-region whole model) ==" type cr
MODEL: MFFN ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 -- y ) LINEAR GELU LINEAR RMSNORM ;  FP-BUILD
s" MODEL: MFFN ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 -- y ) LINEAR GELU LINEAR RMSNORM ;" LMDM-GOLD

\ the same FFN model through the cad.f gate: device golden into OPTIMIZE + PROMOTE evidence row
LMDM-CAD-GATE

\ ============ (B) cross-region MOVEMENT: GELU CONCAT (materialized copy reads a producer buffer) =
s" == GELU CONCAT 4x8 (cross-region movement whole model) ==" type cr
MODEL: MGC ( x:4x8 b:4x8 -- y ) GELU CONCAT ;  FP-BUILD
s" MODEL: MGC ( x:4x8 b:4x8 -- y ) GELU CONCAT ;" LMDM-GOLD

LMDM-END
;package
