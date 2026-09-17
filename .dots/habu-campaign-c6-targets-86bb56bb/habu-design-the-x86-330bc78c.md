---
title: Design the x86_64 backend
status: closed
priority: 2
issue-type: task
created-at: "2026-09-16T13:54:54.724834+03:00"
closed-at: "2026-09-17T17:34:24.612544+03:00"
close-reason: "acceptance met: docs/x86-64.md (landed 2026-09-16, linked from docs/roadmap.md C6) fixes the SysV register mapping, the target-independent compiler half and the registry contract, ELF64 reuse, the cross-build-then-self-host bootstrap and the gate matrix; the eight implementation dots opened 2026-09-17 (habu-write-the-x86-fbaf3086 habu-specify-the-engine-fcbcee25 habu-add-the-x86-56726659 habu-parameterise-the-alloc-7efbe7a1 habu-lower-hir-to-6bf80d33 habu-cross-build-the-d25a959d habu-port-the-ffi-676f745d habu-self-host-the-ccc31e78)"
---

Problem: every Habu binary runs only on arm64; Joel is adding an Intel Linux machine and will build the x86_64 backend, and the design must be settled before code so the arm64 selector, image builders and gates are reused rather than forked. Acceptance: a section in docs/porting.md (or a new docs/x86-64.md) fixing: the SysV AMD64 ABI mapping of the Habu VM registers and the FFI, which of src/compiler/native selection and spill is target-independent and what a backend module must provide through the target registry (habu-bind-compiler-targets-ff970b99), the ELF64 image and relocation model reuse, the bootstrap and fixpoint story on a second architecture, and the gate matrix; it opens the implementation dots. Files: docs/porting.md, docs/roadmap.md section C6. Verify: the section answers each point; the child dots exist. Depends: habu-bind-compiler-targets-ff970b99. Ownership: Joel. Claim: unassigned.
