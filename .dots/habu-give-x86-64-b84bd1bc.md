---
title: Give x86-64 the FFI entry bodies and the primitive table check
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:32:03.247334+01:00"
---

Problem: src/habu/prims.f rows dlopen-entry and dlsym-entry (the FFI's import-table lookup, src/lib/ffi-abi.f DLOPEN-RAW/DLSYM-RAW) have bodies on both kernels (src/habu/habu1.f and src/habu/kernel-x64.f:4095-4096), and both kernel builds run ENGINE-PRIMS:COMPLETE (src/habu/primitive-registry.f), which names a row no section registered, each from its own arch's file: src/habu/habu2.f:2499 and src/arch/x86-64/kernel-object.f:65. The x86-64 writer still uses fixed GOT slots: src/os/linux-x86-64/elf.f:53-54 ELF-DLOPEN-SLOT-OFF and ELF-DLSYM-SLOT-OFF place the table's relocations (:238-239) at rows 1 and 2, where the arm64 writer src/os/linux/elf.f lays the table from the import names the kernel object carries. Acceptance: one arch-neutral call of COMPLETE runs in every kernel build; the x86-64 writer lays its table from IMPORT-NAMES as src/os/linux/elf.f does; ELF-DLOPEN-SLOT-OFF and ELF-DLSYM-SLOT-OFF are gone; an x86-64 build passes COMPLETE. Depends: the x86-64 step (x86-64 is off until Wasm is done). Claim: unassigned.
