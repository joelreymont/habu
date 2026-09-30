---
title: Emit the x86 bodies of the engine writers
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T14:52:44.686639+03:00"
blocks:
  - habu-add-the-engine-ebe5d757
  - habu-model-the-code-c40c75d1
---

Problem: I4c registers its seven engine-writer rows on x86 as refusals, but the Habu interpreter X6 runs needs them.
Acceptance: real bodies in `kernel-x64.f` `DEFINITION,` with I4c's contracts and exit codes: one name-store helper (inline, or at CP inside WINDOW-OPEN with `X64PROV:NATIVE-RANGE,` over it); RECORD-AT,, WINDOW-SPAN, WINDOW-CLOSE, as DOES-RECORD, does (`kernel-x64.f:2000-2052`); the live-name probe of the one-wordlist search (`:491`), `HIDX-ADD,` (`:661`), `PROT-BITS,` (`:2493`), TASK-LIVE-GUARD,; a store of the code pointer into OPEN-CELL.
Files: `src/habu/kernel-x64.f`; `test/x86-64-kernel-definition.f` (package X64K-DEFINITION: one booted image per row outcome, one per refusal code, one `-negative` 21); SUITE x86-64-kernel-definition; `docs/x86-64.md`.
Verify: host `bin/hb --load test/x86-64-kernel-definition.f`; ThinkPad: each image natively with the status its header names.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
