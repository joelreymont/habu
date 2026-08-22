---
title: raw CUresult numbers escape as throw and result codes
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.001729+02:00"
---

Problem: maki/infer/gpt2-model.f:475-476 M-CUDA throws the driver's positive rc; :497-498 and siblings return it through 'catch nip nip' so GPT2:LOGITS (:1309-1326) yields result<n, raw CUresult> while lib/ptx/cuda-driver.f RC0 maps the same failure to E-CUDA; '1 throw' (CUDA_ERROR_INVALID_VALUE) is indistinguishable from any other code. Acceptance: E-CUDA (or a typed result<..., cuda-rc>) everywhere; a test shows a failed launch surface as E-CUDA. Files: maki/infer/gpt2-model.f. Verify: maki host tests; device test target-blocked. Depends: none. Ownership: GPT-2 inference. Claim: unassigned.
