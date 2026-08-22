---
title: maki hygiene
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:26.019118+02:00"
---

Problem: maki/test.f:409-411 stray ';SUITE'; maki/array.f, eval/fixture.f, eval/repair.f open no package against maki/README.md; maki/tensor.f:5-6 error-range text wrong; 537 'zed' (retired Orin host) mentions, 65 headers citing docs/archive/cad-plan.md as authority; maki/README.md never mentions GB10/Spark and 'trains on the GPU' rests on gpu.f GN 4; maki/gpu-emit-test.f:32-36 asserts PTX by substring; maki/transcripts/ (288 lines of LLM transcripts) as suite fixtures for eval/matrix-main.f; maki/infer/resid-kernel.cu (CUDA C) and examples/nanogpt/fetch-gpt2-model.sh inside maki against README:70-72, residency-probe.f:57-58 hardcodes gpt2-model/resid-host.bin and /tmp/hb-resid.cubin. Acceptance: each fixed; the probe kernel and script moved to tools/ and docs/gb10-uma-residency.md; README current. Files: as listed. Verify: maki/test.f. Depends: none. Ownership: maki. Claim: unassigned.
