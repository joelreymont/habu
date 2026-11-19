---
title: Give x86-64 the FFI entry bodies and the primitive table check
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:32:03.247334+01:00"
---

Problem: src/habu/prims.f rows dlopen-entry and dlsym-entry (the FFI's import-table lookup, src/lib/ffi-abi.f DLOPEN-RAW/DLSYM-RAW) have bodies on both kernels (src/habu/habu1.f and src/habu/kernel-x64.f:4095-4096), and both kernel builds run ENGINE-PRIMS:COMPLETE (src/habu/primitive-registry.f), which names a row no section registered, each from its own arch's file: src/habu/habu2.f:2500 and src/arch/x86-64/kernel-object.f:65. Acceptance: one arch-neutral call of COMPLETE runs in every kernel build; an x86-64 build passes COMPLETE. Depends: the x86-64 step (x86-64 is off until Wasm is done). Claim: unassigned.
