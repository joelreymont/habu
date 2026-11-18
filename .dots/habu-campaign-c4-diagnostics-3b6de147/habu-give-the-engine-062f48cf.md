---
title: "Give the engine's raw map failures a record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T07:27:44.182326+01:00"
---

Problem: engine raw maps die with a bare line and no record: src/habu/intern-table.f:73, src/core/checker.f:5133, :9695, :14256, src/core/arena-pages.f:10, src/os/image-bytes.f:24; and the stdin stream's reserve (src/habu/main.f STEP, through src/core/dynamic-storage.f MAP-REJECT) prints its E-MAP record but exits UNCAUGHT 67, which test/program-diagnostics-test.f MAP-DIE-STREAM asserts, where the engine's other refused maps (src/core/include.f MAP-DIE, src/core/compile-reads.f FRESH) exit SYSTEM. Acceptance: each refusal prints an E-MAP record (what was mapped, bytes, the address-space help) and exits SYSTEM (MAP-DIE-STREAM asserts SYSTEM); a test fails first where a trigger exists. Depends: none. Claim: unassigned.
