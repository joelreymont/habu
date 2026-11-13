---
title: Give x86-64 the FFI entry bodies and the primitive table check
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:32:03.247334+01:00"
---

Problem: src/habu/prims.f rows dlopen-entry and dlsym-entry (the FFI's import-table lookup, src/lib/ffi-abi.f DLOPEN-RAW/DLSYM-RAW) have arm64 bodies only (src/habu/habu1.f LIBC-OS DLOPEN-ENTRY/DLSYM-ENTRY), and nothing says so: ENGINE-PRIMS:COMPLETE (src/habu/primitive-registry.f), which names a row no section registered, has one caller, the ARM64 kernel build in src/habu/habu2.f; src/habu/kernel-x64.f KERNEL, runs none. The x86-64 kernel and writer still use fixed GOT slots: src/habu/kernel-x64.f:849 DLSYM, reads src/os/linux/layout.f:33 LINUX-DLSYM-SLOT-OFF ($B8, through src/os/linux-x86-64/target-layout.f:35), and src/os/linux-x86-64/elf.f:54-55 ELF-DLOPEN-SLOT-OFF and ELF-DLSYM-SLOT-OFF place the table's relocations (:237-238) at rows 1 and 2, where the arm64 writer src/os/linux/elf.f lays the table from the import names the kernel object carries. Acceptance: one arch-neutral call of COMPLETE runs in every kernel build; src/habu/kernel-x64.f gives both rows a body that loads the slot by name as habu1.f SLOT, does, and DLSYM, does the same; the x86-64 writer lays its table from IMPORT-NAMES as src/os/linux/elf.f does; LINUX-DLSYM-SLOT-OFF, ELF-DLOPEN-SLOT-OFF and ELF-DLSYM-SLOT-OFF are gone; an x86-64 build passes COMPLETE. Depends: the x86-64 step (x86-64 is off until Wasm is done). Claim: unassigned.
