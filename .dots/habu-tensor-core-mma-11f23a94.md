
RUNG 1 LANDED 2026-07-05 (fable): fragment isolation element-exact; licensed
TF32 kernel device-correct (golden=device-pass:tf32; inverse fault caught);
measured 375.6/393.5/398.5 GFLOP/s - BELOW the f32 cp.async tile (442) and
~21% of Triton; ptxas 38reg/0spill = load/ALU-bound (scalar shared fragment
loads + cvt overhead), not register-bound. VERIFY (climb above the 940 roof)
NOT met yet - dot stays open; path = habu-mma-ldmatrix-fragment (biggest
expected jump), habu-mma-16x64-warp (8x A-reuse), habu-mma-larger-bk +
cad-6 search.
