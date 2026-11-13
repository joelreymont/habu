---
title: Give x86-64 the FFI entry bodies and the primitive table check
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:32:03.247334+01:00"
---

Problem: src/habu/prims.f rows dlopen-entry and dlsym-entry (the FFI's import-table lookup, src/lib/ffi-abi.f DLOPEN-RAW/DLSYM-RAW) have arm64 bodies only (src/habu/habu1.f LIBC-OS DLOPEN-ENTRY/DLSYM-ENTRY), and nothing says so: ENGINE-PRIMS:COMPLETE (src/habu/primitive-registry.f), which names a row no section registered, has one caller, the ARM64 kernel build in src/habu/habu2.f; src/habu/kernel-x64.f KERNEL, runs none. test/x86-64-link-records.f requires tools/native-emit-x64.f, which no longer exists. Acceptance: one arch-neutral call of COMPLETE runs in every kernel build; src/habu/kernel-x64.f gives both rows a body that loads the slot by name as habu1.f SLOT, does; an x86-64 build passes COMPLETE; x86-64-link-records loads. Depends: the x86-64 step (x86-64 is off until Wasm is done). Claim: unassigned.
