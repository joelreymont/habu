---
title: GPT-2 kernels are raw PTX behind TRUSTED mints
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.983498+02:00"
---

Problem: maki/infer/gpt2-tensor-cg.f:108-113 'TRUSTED: EMBED-ABI ( -- span<...> matrix<...> ... ) 1 2 3 4 ;' (and LN/LINEAR/UNEMBED) mint typed signatures from literal register numbers and :115-222 emit raw PTX erased under them (PTXREP:SINK4); maki/infer/gpt2-attention-cg.f:249-271 ROW-REG/CACHE-REG/STATE/APPEND/SCORE/SOFTMAX/OUTPUT are TRUSTED: over ~130 lines of literal PTX with fixed register names; KERNEL: CHECKED (:279-280) checks a chain of trusted stubs. Only GELU-K/RESIDUAL-K use KERNEL:. Five of seven flagship kernels are unchecked while README/docs claim checked kernels; a Q/K swap would certify. Acceptance: the kernels authored in the checked PTX vocabulary (the comments admit runtime-k accumulation is not expressible - that capability is its own dot and blocks this one), or README/docs label the GPT-2 path unchecked until then; the five mints deleted either way. Files: maki/infer/gpt2-tensor-cg.f, gpt2-attention-cg.f, README.md. Verify: maki device suite on a GB10 (cannot run here - record as target-blocked). Depends: the runtime-k capability dot (to be filed by the owner). Ownership: GPT-2 inference. Claim: unassigned.
