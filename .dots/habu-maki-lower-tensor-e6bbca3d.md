---
title: "Maki: lower tensor ops onto Habu-PTX GPU kernels"
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-27T08:06:44.248329+02:00\""
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-fix-ptx-collective-997cfcce
---

Gap #9 (the maki deployment seam). Maki ops (optim/loss/autograd/train) run on CPU float arrays (maki/array.f); they do NOT lower onto the checked Habu-PTX kernels for GPU tensor execution. Build the maki -> Habu-PTX lowering: a maki tensor op (e.g. elementwise add, the SGD step, softmax) selects/instantiates the checked kernel, emits it, and launches it on device (via the proven FFI path tools/ptx/cuda-launch.f, _v2 symbols). Then maki training runs on the GPU.
- Files: maki/ (a lowering + launch driver, FFI boundary), depends on the tile-IR codegen.
- Verify: a maki tensor add runs the checked +. kernel on the GPU and matches the CPU result; then a tensor SGD step on device.
- Dep: tile-IR codegen (gap #1) + maki tensor types (done).

SLICE 1 LANDED 2026-07-04 (fable): elementwise fusion regions lower to generated
flat PTX kernels (maki/lower-ew.f; GELU/SILU device emitters in
lib/ptx/cg-activation.f mirror the host references op-for-op), launch via typed
CUDA bindings with sentinel-guarded readback (maki/lower-launch.f), and
LOWER-GOLDEN (maki/lower-golden.f) - THE FIRST DEVICE-VS-HOST GOLDEN of
CAD-PLAN section 11 - passed on the Orin: GELU->RELU region, 32/32 elements
match the host executor with the host value rounded to the f32 grid under
atol 1e-6 + rtol 1e-5. PTX text testable in-process via the new PTX-L capture
sink (src/arch/ptx/emit.f). NEXT SLICES: (2) reduction class (row-reduce/
softmax-row via cg-collective block schedule + reduction tolerance policy);
(3) matmul class (cg-matmul tiled GEMM + prologue/epilogue EW fusion);
(4) movement (dissolved reshape/transpose/slice lane remap); (5) OPTIMIZE
wiring: cross-region device-buffer handoff (removes E-LLA-INPUT slots-only
cap), broadcast/SCALE/BIAS operands (removes E-LEW-BCAST), multi-output
regions (removes E-LEW-MULTIOUT), LOWER-GOLDEN into the cad.f gate set +
artifact store.

SLICE 2 LANDED 2026-07-04 (fable): row-reduce regions lower to block-per-row
kernels (maki/lower-red.f; RMS/LN/SM bodies mirror host references via the
cg-collective emitters, unforked; prologue AND epilogue EW fusion supported;
one reduction per region, k<=256 v1). Launch plumbing refactored to shared
staging (LLA-EXEC; LRED-RUN grid=rows). LOWER-GOLDEN dispatches on the region
class bit and applies per-class tolerance (EW rtol 1e-5; reduction rtol 1e-4,
justified from k*2^-24 accumulation + ex2.approx ULP). Device-proven on the
Orin from the pushed tree: RMSNORM/LAYERNORM/SOFTMAX-ROW/GELU->RMSNORM 4x8
all V-PASS 32/32. NEXT: slice 3 matmul (2D tile grid + K-loop via a cg-matmul
emitter, prologue/epilogue fusion, third launch shape in the staging), then
movement, then OPTIMIZE wiring.

SLICE 3 LANDED 2026-07-04 (fable): matmul/linear regions lower to a 16x16
correctness tile (one elem/thread, runtime K-loop, bounds-masked; LINEAR bias
after the K-loop; unary EW epilogue on the accumulator). Device-proven on the
Orin from the pushed tree: MATMUL 8x8, LINEAR 4x8@8x16+bias, LINEAR GELU
epilogue - all V-PASS 64/64 elems. Tolerance: matmul rtol 1e-4 (K<=256,
K*2^-24 bound). FINDING: the task premise was wrong - FP-BASE-FUSE? DOES fuse
EW prologues into contractions (GELU MATMUL = one region); v1 fails closed
E-LMM-PROLOGUE. Test note: LINEAR GELU synthetic inputs land post-bias in
GELU's identity range - vary GA-FILL scale in a later slice so the epilogue
golden exercises the curved region (emitter numerics separately proven by
slice 1). Perf path (register-blocked tiles, MMA) = habu-tiled-gemm-codegen +
CAD-PLAN 8.1; this slice is the correctness substrate. NEXT: slice 4 movement
(index-remap dissolution into existing kernel bodies), slice 5 OPTIMIZE wiring
(cross-region buffers removing E-LLA-INPUT, broadcast operands, multi-output,
LOWER-GOLDEN into cad.f gates + store).

SLICE 4 LANDED 2026-07-04 (fable): movement lowering, both legs of CAD-PLAN 6.3,
device-proven on the Orin (6/6 LOWER-GOLDEN V-PASS, verbatim). (A) DISSOLVED
MVV-FREE movement folds into the reading kernel's index math: a new pure resolver
(maki/move-view.f, owns -5201..-5204) turns a dissolved movement operand into a
source slot + constant element offset, and lower-ew/red/mm each bake ONE
`add.u64 %rdN, %rdN, r0*cols*4` (reshape=0, slice=r0*cols) on the operand base
before the UNCHANGED op body (LEW/LRED/LMM-APPLY-VIEWS); the launch uploads the
source buffer and unifies on per-input element sizing (maki/lower-launch.f
LLA-STAGE-IN). Device V-PASS: SLICE:0..2 GELU (EW 16/16), SLICE:1..3 RMSNORM
(RED 16/16), SLICE:2..6 MATMUL 8x8@8x16 (MM 64/64). (B) a MATERIALIZED movement
region lowers to a copy kernel (maki/lower-move.f, owns -5205..-5212): transpose =
div/rem row remap (coalesced write, strided read), slice = offset copy, concat =
two-source branch, gather = f32-index round (add.f32 +0.5 / cvt.rzi, mirrors
EX-BUILD-IDX) + indexed rows; LMV-RUN launch (1-2 buffers, p_a/p_b/p_n, 1D grid,
LG-MOVE? routes it, exact tolerance). Device V-PASS: TRANSPOSE 4x8 (32/32),
SLICE:1..3 of 4x6 (12/12), GATHER 8x8 (32/32). FAIL CLOSED + dotted: staged
transpose folded into a compute region (E-MVW-STAGED, dot habu-maki-fold-staged);
a movement model-output the planner leaves un-materialized (E-LMV-NOOUT, a
fusion-plan.f gap, dot habu-maki-fusion-plan - tests force mat=1 or use a
multi-use fan-out); a movement whose source is a cross-region/interior node
(E-MVW-SRC / E-LMV-INPUT, dot habu-maki-cross-region, the slice-5 buffer handoff).
Note: GA-FILL zeroes gather indices so the gather golden gathers row 0 only (still
catches an identity kernel; strengthen via dot habu-maki-strengthen-gather). Tests:
maki/lower-mv-test.f (capture text + fail-closed, wired into maki/test.f = 65),
maki/lower-mv-device-test.f (Orin, not gated). NEXT: slice 5 OPTIMIZE wiring +
the four dots above.

SLICE 5 LANDED 2026-07-04 (fable): whole-model device execution + the device golden
into the CAD gate. LOWER-MODEL-RUN (maki/lower-launch.f) executes EVERY region of the
forward IR on device in topo (materialized-node) order: each region's output stays in a
context-scoped device buffer (MDL-BUF, keyed by node) and a downstream region whose input
names that producer node BINDS the buffer instead of uploading, removing the slots-only cap
for the whole model (E-LLA-INPUT stays as the single-region LOWER-GOLDEN guard). Context is
opened once; per-region REGION_<rid> cubins register via MDL-CUBIN! and modules load/unload
per region (devptrs are context-scoped, so buffers persist across module loads). LOWER-MODEL-
GOLDEN (maki/lower-golden.f) compares the FINAL model output vs the host executor under a
COMPOSED tolerance: the device carries f32 at every region boundary while the host stays f64
and narrows once, so per-region class rtols/atols are SUMMED (first-order error propagation,
a sound upper bound - maxing would understate a deep chain) = 2*matmul + 1*row-reduce for the
FFN. The materialized movement copy now accepts a cross-region producer node (maki/lower-move.f
LMV-REF-ROWS/COLS; E-LMV-INPUT repurposed to reject an un-materialized interior node), which
CLOSES habu-maki-cross-region. Gate: maki/cad.f GOLDEN-GATE-INTO precedence external-artifact >
DEVICE model golden (present + cubins registered + lowerable) > host self-consistency; golden.f
stays device-free; PROFILE stays honest not-run; PROMOTE evidence records golden=device-pass
(maki/store.f EVID-PUT-G). MDL-CUBINS-READY?/MDL-LOWERABLE? make an on-device OPTIMIZE without
cubins (or a non-lowerable model) fall back to host self, so the 66-suite gate is green ON the
Orin too. Device-proven on the Orin (verbatim): FFN LINEAR GELU LINEAR RMSNORM 4x8 whole-model
golden V-PASS 32/32 over 3 cross-region regions; OPTIMIZE golden device-pass + PROMOTE evidence
certify=pass|golden=device-pass|gradcheck=pass|profile=not-run; GELU CONCAT cross-region movement
V-PASS 64/64. Tests: maki/lower-model-test.f (host, wired into maki/test.f = 66),
maki/lower-model-device-test.f (Orin, not gated). Still fail-closed + dotted/invariant: broadcast
operands (E-LEW-BCAST) and multi-output regions (E-LEW-MULTIOUT) remain slice-5-out-of-scope caps;
E-MVW-SRC stays a CORRECT invariant (a dissolved movement's source must be a model input slot - a
node source is either a non-foldable same-region interior register value or belongs to a copy
region, so it is never a base-offset fold; not a gap). NEXT: SAXPY retirement (the last hardcoded
kernel path) + CAD-PLAN 8.1 register-blocked GEMM (habu-tiled-gemm-codegen), plus the two movement
planner dots habu-maki-fusion-plan + habu-maki-fold-staged (NOT touched by slice 5).
