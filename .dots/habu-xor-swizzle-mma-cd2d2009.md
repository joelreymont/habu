---
title: XOR-swizzle MMA shared layout (pad-free)
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T22:11:50.777573+02:00\""
---

Optional refinement from habu-mma-larger-bk-1ae1c6b2 (2026-07-15): MMA-PAD=8 (row-stride padding) reduces the ldmatrix shared-bank conflict from 16-way to near-free and is the committed win (+54%), but costs 4 KiB extra static shared per tile (36 vs 32 KiB; 66 KiB dyn for BK=64). A true XOR swizzle (address ^= (row & mask) << shift on the shared store/ldmatrix load index math) is textbook fully-bank-free at ZERO padding cost - frees shared budget for larger tiles/stages. Only worth doing when chasing the next GFLOP/s step (after 918 MHz re-measure + default flip); measure honestly vs the pad config, keep element-exact green, fail-closed legality unchanged. Files: lib/ptx/cg-mma.f (index math emitters), mma-gemm-check config rows. Ownership: ptx MMA layout.

Claim: agent=xorswz workspace=.jj-ws/fable-xorswz machine=spark (owns lib/ptx/cg-mma.f index-math + mma-gemm-check/emit-diff/gemm-bench rows; the session TIMING lane; wave3 fence verified stale — phantom legs resolved 2026-07-17)
