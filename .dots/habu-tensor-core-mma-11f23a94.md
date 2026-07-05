
RUNG 1 LANDED 2026-07-05 (fable): fragment isolation element-exact; licensed
TF32 kernel device-correct (golden=device-pass:tf32; inverse fault caught);
measured 375.6/393.5/398.5 GFLOP/s - BELOW the f32 cp.async tile (442) and
~21% of Triton; ptxas 38reg/0spill = load/ALU-bound (scalar shared fragment
loads + cvt overhead), not register-bound. VERIFY (climb above the 940 roof)
NOT met yet - dot stays open; path = habu-mma-ldmatrix-fragment (biggest
expected jump), habu-mma-16x64-warp (8x A-reuse), habu-mma-larger-bk +
cad-6 search.

RUNG 1b MEASURED NEGATIVE 2026-07-05 (minion-ldmx): habu-mma-ldmatrix-fragment
CLOSED with a falsification. The 3-mode fragment-feed ablation (cg-mma.f
MMA-LMODE: scalar+cvt / scalar raw no-cvt / ldmatrix.x4 A + raw B no-cvt), all
element-exact (mma-probe MP-LDM-ALL, mma-gemm-check 64^3+128^3 every mode, tf32
golden green): cvt-drop FLAT, ldmatrix ~1.2% SLOWER (370.0/388.9/394.3 GFLOP/s;
43 vs 38 reg, 0 spill). The rung-1 "load/ALU-bound" diagnosis is WRONG: at 16x32
warp tile / 4x A-reuse the MMA is issue/dependency-bound (mma waits on the B
loads just before it; BK=32 bar.sync cadence), invariant to load flavor. Default
stays mode 0 (exact-RNE golden unchanged); the proven ldmatrix mechanism stays
selectable. Path reordered: habu-mma-16x64-warp (8x A-reuse, more independent
mma per fragment) is now the primary lever, then habu-mma-larger-bk (fewer
syncs; swizzled Bs unlocks B-side ldmatrix); cad-6 searches warp-tile/BK, not
load flavor. Record: docs/eval-triton.md step 3c.
