---
title: Give x86-64 the dlopen-entry and dlsym-entry bodies
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:32:03.247334+01:00"
---

Problem: src/habu/prims.f rows dlopen-entry and dlsym-entry (the FFI's import-table lookup, src/lib/ffi-abi.f DLOPEN-RAW/DLSYM-RAW) have arm64 bodies only (src/habu/habu1.f LIBC-OS DLOPEN-ENTRY/DLSYM-ENTRY); an x86-64 build stops in ENGINE-PRIMS:COMPLETE (src/habu/primitive-registry.f) with 'prims: no backend body for specified primitive dlopen-entry'. Acceptance: src/habu/kernel-x64.f gives both a body that loads the slot by name as habu1.f SLOT, does, and an x86-64 build passes COMPLETE. Depends: the x86-64 step (x86-64 is off until Wasm is done). Claim: unassigned.
