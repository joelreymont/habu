---
title: Design the x86_64 backend
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:54:54.724834+03:00"
---

Problem: every Habu binary runs only on arm64; Joel is adding an Intel Linux machine and will build the x86_64 backend, and the design must be settled before code so the arm64 selector, image builders and gates are reused rather than forked. Acceptance: a section in docs/porting.md (or a new docs/x86-64.md) fixing: the SysV AMD64 ABI mapping of the Habu VM registers and the FFI, which of src/compiler/native selection and spill is target-independent and what a backend module must provide through the target registry (habu-bind-compiler-targets-ff970b99), the ELF64 image and relocation model reuse, the bootstrap and fixpoint story on a second architecture, and the gate matrix; it opens the implementation dots. Files: docs/porting.md, docs/roadmap.md section C6. Verify: the section answers each point; the child dots exist. Depends: habu-bind-compiler-targets-ff970b99. Ownership: Joel. Claim: unassigned.
