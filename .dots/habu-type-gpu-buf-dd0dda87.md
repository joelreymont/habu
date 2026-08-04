---
title: Type GPU buffer internals
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T06:04:30.690404+02:00"
---

Why: GPU:buffer currently needs TRUSTED raw-cell projections because checked Habu cannot express a dynamically allocated owner header with role-typed cuda-devptr and CAD-NUM:alloc-byte-len fields, or advance a cuda-devptr by a checked CAD-NUM:byte-off. Behavior and interfaces: add the checker/core storage and role-arithmetic capability needed for a private linear GPU owner to allocate, initialize, read, and release those typed fields and perform bounded cuda-devptr byte displacement; add no public GPU API. Dependencies: none. Owned result: checker support plus migration of the exact GPU buffer representation boundaries that cite this dot. Package or namespace owner: checker typed storage and CUDA role arithmetic; GPU remains the sole consumer API owner. Acceptance: GPU:ALLOC, GPU:UPLOAD, and GPU:FREE certify unchanged while their raw length/offset/header and cuda-devptr-add TRUSTED boundaries are removed; negative checker fixtures reject role-mismatched field stores and arithmetic; the checker suites and maki/gpu-buffer-test.f pass. Owning-path check: before the capability, a minimal checked private GPU owner with a cuda-devptr field, CAD-NUM:alloc-byte-len field, and cuda-devptr plus CAD-NUM:byte-off fails certification on the production load path; after it, that fixture and maki/gpu-buffer-test.f certify and run.
