---
title: Share one x86 target-layout package
status: open
priority: 3
issue-type: task
created-at: "2026-09-30T13:02:53.346419+03:00"
---

Problem: the x86 target layout (`src/os/linux-x86-64/layout.f`) is replayed inside three packages so its constants do not collide with the host's globals: X64BOOT (`src/habu/boot-x64.f:32`), X64KERNEL (`src/habu/kernel-x64.f:45`) and X64LAYOUT (`src/os/linux-x86-64/elf.f`, from X4a habu-write-and-link-f6e6017f).
Acceptance: one package owns the replayed x86 target layout; the three users read it through that package; no other replay remains; host globals stay untouched (the seam suite runs on the macOS gate).
Files: `src/os/linux-x86-64/elf.f`, `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, the owning file, `docs/x86-64.md`.
Verify: ThinkPad: every x86 suite, every `hb-x64-*` image natively, the x64-routines manifest loop bad=0; byte-identical images before and after.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
