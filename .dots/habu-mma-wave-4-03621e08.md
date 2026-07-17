---
title: "MMA wave-4: roofline verdict for the mma-issue floor"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T12:52:16.863117+02:00\""
---

Residual from habu-mma-wave-3-f4d51eb6 (2026-07-17, commit 58faceba): MMM-WIDE-B-M4-S1 (3026.6 GFLOP/s = 1.60x Triton at 2048^3/918MHz) is now ~21% mma-issue-bound by DCE-safe ablation (tools/ptx/mma-ablate.f) and at 93% of its own quarter-B ceiling - the B-feed program is exhausted. Before any further kernel work, produce an honest ROOFLINE VERDICT: (1) compute the sm_87 Orin NX tf32 mma.sync theoretical peak at 918MHz from the architecture facts (tensor core count per SM, SM count, tf32 m16n8k8 throughput per clock - document sources in docs/kernel-principles.md), (2) place 3026.6 against that peak and against the measured quarter-B/single-mma ablation ceilings, (3) verdict: either the kernel is within noise of the tf32 issue roof (then CLOSE the GEMM perf program with the documented roof and shift focus to the autotuner/default-flip dots), or a quantified instruction-level lever remains (e.g. m16n8k16-equivalent scheduling, dual-issue across the 4 M-frags, accumulator register pressure) - then dot THAT lever with the measured headroom. Explicitly out of scope: fp16/bf16 instruction shapes (different numerics contract than the tf32-for-f32 eval policy - if the analysis shows fp16 is the only remaining lever, record it as a USER-GATED policy question, do not implement). Mostly host/desk work; a short zed session only if a discriminating micro-benchmark is needed (mma-probe-style issue-rate kernel, sole-owner protocol, 918MHz pin, never ncu). Acceptance: docs/kernel-principles.md roofline section with the peak derivation + verdict; either the program-close recommendation or the next lever dot minted with measured headroom; perf-rows note if a micro-benchmark ran. Files: docs/kernel-principles.md, LESSONS.md, optionally tools/ptx/mma-probe.f + perf-rows.tsv. Ownership: kernel perf.

Claim: agent=wave4 workspace=.jj-ws/fable-wave4 (roofline verdict - desk work; sole zed owner if a micro-benchmark is needed)
