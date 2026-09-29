---
title: Run emitted x86 routines natively as a fixture family
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.424177+03:00"
---

Problem: `test/x86-64-peer-image.f` executes one routine; new forms have only pinned bytes (`test/compiler/x64-emit.f`). Every other C leaf verifies through this family.
Acceptance: `test/x86-64-peer-routines.f` builds one ELF per HIR fixture through the real rows (`NBACK:EMIT` at a placement, as `X64PEER:PEER-ROUTINE` does), each with a positive and a wrong-expectation negative; a manifest lists expected exit statuses; on the ThinkPad every positive exits 0 and every negative its stated code.
Files: `test/x86-64-peer-routines.f`, `test/gate-stdlib-cases.f` (host row builds only).
Verify: spark `bin/hb --load test/x86-64-peer-routines.f`; ThinkPad runs `$TMP/x64-routines/*` and compares the statuses with the manifest.
Depends: none.
Route: Alder (shared: test/gate-stdlib-cases.f).
Ownership: krait (Intel lane).
Claim: unassigned.
