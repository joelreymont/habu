---
title: Build staged GPU compiler
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:09.596072+02:00"
---

Full context: design sections 8 and 14 GPU Waves A-E replace string-first PTX generation with RIR, KIR, GIR, and PTXIR2 while preserving the checked model boundary. Required result: structured PTX, elementwise, reduction/softmax, GEMM/MMA, planner/tuner cutover, and old-emitter retirement. Acceptance: design section 21 GPU criteria pass without editing maki/infer while Spark agents are active.
