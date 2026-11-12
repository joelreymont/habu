---
title: Name the container readers by their subject
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T15:52:07.038040+02:00"
---

Problem: tools/elf-read.f (package ELF-READ) and tools/macho-read.f (package MACHO-READ, :6) put an operation in a package name, which docs/forth-card.md section 1 refuses: a subject's reading and writing words share its package. The writers are package ELF (src/os/linux/elf.f:10) and package MACHO (src/os/macos/macho.f:26). Acceptance: the readers' words live in their subject's package and every caller (tools/image-size-lib.f and the MACHO-READ callers) uses those names; a tool build and the size report run as before. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-readers.
