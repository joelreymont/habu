---
title: "Share the Linux ELF writers' program headers"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T08:41:48.809257+01:00"
---

Problem: src/os/linux/elf.f and src/os/linux-x86-64/elf.f each write ELF64 program headers and their offsets by hand (ELF-PHDRS, and ELF-REGION-PHDR, in both; the x86 header records its own $190 against the other's $158, which the code-region change had to edit in both), though their differences are only what the architecture owns (e_machine, the interpreter, the GOT relocation type, x86's DATA load). x86's engine boot (src/habu/boot-x64.f CODE-REGION,) maps anonymous memory fixed over the region its image's own load already reserves. Acceptance: one writer of the program headers that both machines call with their facts; the x86 engine boot maps nothing over its region load; both machines' images unchanged but for that. Depends: none. Claim: unassigned.
