---
title: Prove GB10 inference target
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:40:51.245945+02:00"
---

Problem: src/arch/ptx/emit.f defaults to sm_87 while this machine is GB10 compute capability 12.1 and the production toolchain names it sm_121a; inference claims can silently assemble the wrong target. Result: the inference owning path obtains TARGET:SM121A from the probed active target, sets both PTX header and PTXTC assembler arch to sm_121a with the target-required PTX version, and runs the exact selected embedding, normalization, matmul, GELU, residual, softmax, and existing attention primitives on GB10. Do not change unrelated Orin defaults or add a compatibility target. Owner: inference target selection and focused selected-kernel matrix only. Production red: LOWER-MODEL-RUN and several emitters inherit sm_87 by default. Acceptance: the real GB10 probe reports 12.1 and sm_121a; emitted headers and ptxas argv agree; every selected kernel assembles, loads, executes, and matches its landed golden or GPT2-REFERENCE probe; forcing sm_87 on this production path fails the target assertion. Smallest owning check: bin/hb --load maki/infer/gb10-target-test.f on DGX Spark. Claim: unassigned.
