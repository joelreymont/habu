---
title: Checked ATTENTION benchmark producer (attention-bench)
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T17:08:46.971740+02:00\""
---

The perf registry's ATTENTION row is a value-0 WAIVER because no benchmark producer exists - only the emitter (tools/ptx/attention-cg.f, lib/ptx/cg-attention.f, design note ~8 GFLOP/s at N=128 D=64). Retiring the waiver with a measured row needs a new checked tools/ptx/attention-bench.f (+ test in the ptx-toolchain suite): self-emit ATTN via the fusion-emit.f/gpu.f precedent (private per-run PTXTC root, fail-closed E-PTX-EMIT), ptxas-assemble, alloc Q/K/V/O (params pQ@0/pK@8/pV@16/pO@24/pN@32/pD@36, grid=N block=N, N<=128 D<=64), CUDA-event time, report GFLOP/s_x1000 in perf-rows.tsv row format (device tag orin-nx-25w per the 2026-07-14 rescope of habu-perf-registry-re-6be03867). Host legs: emit proof + row-format test; device launch leg gated on CUDA:OPEN? with recorded SKIP. The waiver retirement itself (registry row + PRT-COMMITTED-TESTS flip) belongs to 6be03867 once this tool lands. Files: tools/ptx/attention-bench.f (+test), gate tables, FILEMAP.md. Verify: lint-libs ptx-toolchain, lint-tools, host/filemap lints. Ownership: ptx bench tools.

Claim: agent=attnbench workspace=.jj-ws/fable-attnbench

PRODUCER LANDED 2026-07-14 (attnbench worker, "ptx: checked attention benchmark
producer"): tools/ptx/attention-bench.f + attention-bench-test.f, wired into
the ptx-toolchain suite (spawned + inline emit-proof). Reuses
PTXFE:BUILD-KERNEL (fusion emit prelude is a superset of attention-cg's
requires, verified by clean ATTN PTX emission). FLOP model 4*N*N*D + 5*N*N
documented in-file; bytes reported as Q+K+V+O footprint 16*N*D. Emit half
proven host-side; the DEVICE LAUNCH LEG IS UNVERIFIED (no CUDA on the dev
host) - this dot stays active until the first on-device run
(`bin/hb --load tools/ptx/attention-bench.f` on zed, 25W, pinned clock), which
also supplies the measured orin-nx-25w ATTENTION row that retires the WAIVER
under habu-perf-registry-re-6be03867 (with the PRT-COMMITTED-TESTS flip).
