---
title: Resolve x86 entry cells and dispatch the writer
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:29.008645+03:00"
blocks:
  - habu-link-the-shadow-74e41be7
  - habu-add-the-x86-a8bf9973
---

Problem: nothing resolves the entry cells in an x86 image, and `NATIVE-EMIT:WRITE` has no x86 arm.
Acceptance: `ENGINE-MAIN:XT-CELL`/`APP-ENTRY:XT-CELL` resolution, the `.names` sidecar, the `NATIVE-EMIT:WRITE` x86 arm (over K3's require set) dispatching to the linker; `docs/x86-64.md` gains the write-time link section; `docs/porting.md` states that a port is seam + kernel + link arm with no cold route.
Files: `src/habu/link-x64.f`, `tools/native-emit.f`, `docs/x86-64.md`, `docs/porting.md`.
Verify: spark writes a linked image; ThinkPad `readelf -l`; X5 executes it.
Depends: habu-link-the-shadow-74e41be7 (X4c), habu-add-the-x86-a8bf9973 (K2: declares `ENGINE-MAIN:XT-CELL`, so X5 never waits on lane I). Serialise with K3 on `tools/native-emit.f`.
Route: Alder (shared: tools/native-emit.f, docs/porting.md).
Ownership: krait (Intel lane).
Claim: unassigned.
