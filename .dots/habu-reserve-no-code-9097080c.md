---
title: Reserve no code region in a stripped Linux program
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T20:55:26.355830+01:00"
---

Problem: src/os/linux/elf.f ELF-PHDRS (:220-226) writes ELF-REGION-PHDR, (:212-213), a PF-RW PT_LOAD of the whole REGION, into every Linux image, so a stripped program, which maps no code region (arm64 START,, and x86-64 once habu-map-no-alias-1df4d3ed lands), still reserves 35 MB at REGION-OFF; and src/habu/boot-x64.f OPEN, and STUBS, (:880, :903) lay down the REGION-BAD failure text no stripped start jumps to. Acceptance: a stripped Linux program's /proc/self/maps shows no region reservation on either arch, its image carries no REGION-BAD text, and an engine's image is byte-identical to before. Depends: habu-map-no-alias-1df4d3ed. Claim: unassigned.
