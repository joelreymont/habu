---
title: Check the x86 kernel for every primitive row
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T08:06:23.532686+03:00"
---

Problem: `ENGINE-PRIMS:COMPLETE` (the completeness gate the ARM64 engine runs over `src/habu/prims.f`) appears in `src/habu/kernel-x64.f` only in comments; `KERNEL,` never calls it. So a row with no x86 body and no refusal goes unnoticed until a captured program calls it. Inventory on master `02e6d5b3`: `does-patch`, `native-unit-publish` and `unit-compile-run` (and the `(` row) have no x86 registration at all.
Acceptance: building the x86 kernel fails, naming each row, when any `prims.f` row has neither an x86 body nor an explicit refusal; the rows found above get an explicit x86 refusal (exit 76) until their bodies land (I7 for the definer rows), or their bodies if that is smaller.
Files: `src/habu/kernel-x64.f` (`KERNEL,`), `test/x86-64-boot-harness.f` if the check belongs to the harness build, `docs/x86-64.md`.
Verify: a booted kernel suite builds; deleting one row's registration in a scratch copy makes the build fail naming it.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
