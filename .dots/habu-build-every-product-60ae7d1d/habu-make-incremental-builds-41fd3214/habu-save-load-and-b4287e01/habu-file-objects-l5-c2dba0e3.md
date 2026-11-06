---
title: "File objects L5: rebuild what changed"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.556348+02:00"
blocks:
  - habu-file-objects-l4-62887595
---

Problem: a warm build must recompile a changed file and every file that depends on it, and only those once L6 lands. Acceptance: the three load words record each request, performed or already loaded, between new REQUIRE-REG:RECORD-LOADS and REQUIRE-REG:;RECORD-LOADS, read by REQUIRE-REG:LOAD-COUNT and REQUIRE-REG:LOAD-ROW@ ( n -- ptr u8 n bool ) (renamed into SOURCE's input log by jerry's merge, design-source section 3.1); freshness as design section 2 item 6 (dependencies by request, reference and spelled name; wordlist counter and loader state at the start; laid strings not already in the table), the subtree checked before any of it links; the build machine linking -> compiling -> restart of item 9. Design: .out/carl-file/design-fobj.md section 2 items 6, 7, 9. Files: src/core/include.f, src/habu/linker.f (or its object file), test/file-objects.f. Verify: edit a.f, both compile; edit b.f, b.f compiles (whole build until L6); a require only for its side effect, dropped by the first requirer, still loads; an early file grown by one cell leaves later objects identical apart from their recorded hashes. Depends: L4. Ownership: carl (lead), worker lane. Claim: unassigned.
