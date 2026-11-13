---
title: "Find the FFI's dlopen and dlsym through the import table"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T18:25:16.317552+01:00"
---

Problem: src/lib/ffi-abi.f DLOPEN-RAW and DLSYM-RAW call the GOT cells src/os/macos/layout.f DLOPEN-SLOT/DLSYM-SLOT and src/os/linux/layout.f name by fixed offsets (row 0 and 1; $B0 and $B8), while the kernel finds every other import by name through IMPORT-ROW (src/habu/habu1.f SLOT,), so a tree that reorders the import table moves the kernel's reads but not FFI's. Acceptance: the FFI's dlopen and dlsym entries come from the import table by name; the layouts' fixed slot words are gone; Maki's worker, which calls FFI:DLOPEN, builds. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ffi.
