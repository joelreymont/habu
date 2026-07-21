---
title: Migrate remaining PTX bench/sweep CUDA consumers
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T06:39:53.640531+02:00\""
---

Follow-up from the PTX lifecycle migration (771d921a): the load-once PTXBENCH consumers (gemm-bench, attention-bench, bandwidth-lib, mma-probe/ablate/profile/gemm-check) and autotune-sweep.f still use happy-path cleanup. The enabling PTXBENCH:OWN-* transfer API landed; migration is now mechanical for the bench runners (wrap run in SCOPE, own ctx+module+buffers, delete the DEVICE-FREE/UNLOAD/CLOSE lists). autotune-sweep additionally needs an MX-OWN buffer-ownership API in shared mma-exact-lib.f (also consumed by mma-gemm-check) and a local->variable refactor of AT-SWEEP/SW-CANDIDATE before it can wrap. Validation is correctness smoke launches only - never timing runs as proof; one GPU timing lane discipline applies if a sweep must run.

Claim: agent=ptxbench2 workspace=.jj-ws/fable-ptxbench2 machine=spark (owns migrating the remaining PTXBENCH consumers + autotune-sweep onto CUDA-SCOPE incl. the MX-OWN api; correctness smoke only, no timing proof)
