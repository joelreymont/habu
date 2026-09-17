---
title: Add the x86_64 Linux OS seam and ELF64 writer
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.539125+03:00"
---

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): src/os/linux/{layout,elf,sys,repl-term,sign}.f carry aarch64 image constants, syscall numbers and an ELF writer; x86_64 needs the same file set with the same word names so the image builders select a seam instead of branching. Acceptance: src/os/linux-x86-64/ with layout.f (the same DATA and guard-page model), elf.f (ELF64 for EM_X86_64, program headers, the relocation site kind for mov r64, imm64), sys.f (x86_64 syscall numbers and the syscall instruction sequence), repl-term.f and sign.f, selected by the target predicates docs/porting.md describes; a test that writes a minimal x86_64 ELF from the arm64 engine and checks its headers by field; docs/porting.md gains the x86_64 seam row. Files: src/os/linux-x86-64/, src/habu/aot-lib.f (site kind), formal/Common/Reloc.v (the vector for the new site kind), docs/porting.md, test/. Verify: the ELF test on arm64; the file runs on the Intel machine once a backend emits code. Depends: none. Ownership: Joel (x86_64 lane). Claim: unassigned.
