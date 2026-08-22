---
title: catch drop in the GPU unwind and tests
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.012861+02:00"
---

Problem: maki/gpu.f:131 '[: CUDA-SCOPE:UNWIND ;] catch drop throw' discards an unwind failure; maki/examples/nanogpt/data-loader-test.f:294 and maki/infer/safetensors-test.f:403 'catch drop'; docs/forth.md:932-934 forbids the form; gpu-session.f:75-76 already shows FIRST-CODE composition. Acceptance: each composes codes; a test provokes an unwind failure and sees it. Files: maki/gpu.f, the two tests. Verify: maki/test.f. Depends: none (moot for gpu.f if it is deleted first). Ownership: maki. Claim: unassigned.
