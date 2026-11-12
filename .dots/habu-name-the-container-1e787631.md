---
title: Name the container readers by their subject
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T15:52:07.038040+02:00"
---

Problem: tools/elf-read.f (package ELF-READ) and tools/macho-read.f (package MACHO-READ, :6) put an operation in a package name, which docs/forth-card.md section 1 refuses: a subject's reading and writing words share its package. The writers are package ELF (src/os/linux/elf.f) and package MACHO (src/os/macos/macho.f), which the engine bakes (src/habu/link-data.f:23-26), and card section 6 (docs/forth-card.md:507-508) lets a baked package have one owning file only: a tool that reopens ELF or MACHO shares the writers' private wordlists, so a later private of the same name makes `hb build --size-report` exit 78 (E-DUPLICATE-DEFINITION). Acceptance: the readers' words live in the owning files, src/os/linux/elf.f and src/os/macos/macho.f, the tool files are gone, and every caller (tools/image-size-lib.f, tools/imgdump.f) uses ELF: and MACHO: names; the size report and imgdump print the same output; the engine's growth is measured. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-readers2.
