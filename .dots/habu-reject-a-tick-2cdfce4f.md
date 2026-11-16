---
title: Reject a tick the checker admits but cannot compile
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T07:48:23.402121+01:00"
blocks:
  - habu-tell-a-ticked-351f6635
---

Problem: src/core/checker.f BTICK-TOK asks REFUSE-UNDEFINED only on `sym 0= rec 0= and`, so a body tick of a spelling that binds a checker symbol but has no record passes the checker and is rejected later by the compiler (src/habu/t0.f DO-TICK → NOT-FOUND) with a record that names no definition. The keywords among such spellings (`>r`, `s\"`, …) get the keyword record from W195 (dot 351f6635); whether any non-keyword spelling binds a symbol with no record is unknown. Acceptance: every tick the checker admits compiles, or the checker rejects it at the tick with a true record; a test pins one such spelling if any exists. Depends: 351f6635. Claim: unassigned.
