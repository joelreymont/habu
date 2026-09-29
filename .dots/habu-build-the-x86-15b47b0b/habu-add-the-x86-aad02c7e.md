---
title: Add the x86-64 code layer
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.517403+03:00"
---

Problem: `ASM-SINK`, labels and forward rel32 fixups have no home (`src/os/linux-x86-64/sys.f:15-22`; `docs/porting.md:29-35`).
Acceptance: `src/arch/x86-64/icode.f` (package `X64CODE`): a `BUF` sink, `ASM-SINK ( -- ptr u8 )`, labels with rel32/rel8 fixups and `MOVABS` label sites, refusal on unresolved labels; `test/x86-64-emit.f` and `test/x86-64-peer-image.f` bind to it instead of their private sinks. Discharges cross-build obligation (1).
Files: new `src/arch/x86-64/icode.f`, `test/x86-64-emit.f`, `test/x86-64-peer-image.f`.
Verify: spark `bin/hb --load test/x86-64-emit.f` and `bin/hb --load test/x86-64-peer-image.f`; ThinkPad peer statuses 0 (`hb-x64-peer`) and 21 (`hb-x64-peer-negative`) unchanged.
Depends: none.
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
