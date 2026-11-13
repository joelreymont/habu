---
title: "Find the FFI's dlopen and dlsym through the import table"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T18:25:16.317552+01:00"
---

Problem: src/lib/ffi-abi.f DLOPEN-RAW and DLSYM-RAW call the GOT cells src/os/macos/layout.f DLOPEN-SLOT/DLSYM-SLOT and src/os/linux/layout.f name by fixed offsets (row 0 and 1; $B0 and $B8), while the kernel finds every other import by name through IMPORT-ROW (src/habu/habu1.f SLOT,), so a tree that reorders the import table moves the kernel's reads but not FFI's. Acceptance: the FFI's dlopen and dlsym entries come from the import table by name; the layouts' fixed slot words are gone; Maki's worker, which calls FFI:DLOPEN, builds. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ffi.

Acceptance (2026-11-13, from the lane): src/os/linux/layout.f now defines nothing; its constants live in src/os/linux/layout-constants.f. Linux keeps one layout.f holding them, as macos/layout.f does, and layout-constants.f goes with its loaders (src/habu/boot-files.f, tools/imgdump.f, tools/lint/shadow-lint.f, src/os/linux-x86-64/target-layout.f, docs/xcomp.md, src/os/AGENTS.md).
