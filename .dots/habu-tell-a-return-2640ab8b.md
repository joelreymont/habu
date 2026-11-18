---
title: Tell a return-row cycle from an unbalanced row
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T14:04:03.365098+01:00"
---

Problem: ': G30 ( R [ R -- R | U -- U ] -- R ) dup >r execute r> drop ;' is rejected with 'return stack: the return row does not balance at exit', '= expected: (nothing)' and '= found: (nothing)': the return row would contain itself (U-ROW CELL-OCC?), but RS-CAPTURE (src/core/checker.f ~:5785) prints the whole return rows trimmed until they read alike. Acceptance: the rejection names the return row that would contain itself, with expected and found that differ; test/checker-soundness-suite.f CS-K100 is extended to pin it and fails first. Depends: none. Claim: unassigned.
