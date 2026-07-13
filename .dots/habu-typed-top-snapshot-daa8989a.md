---
title: "typed-top: snapshot/AOT effect-row parity"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T10:38:59.771262+02:00"
---

Sub-dot 6 of docs/typed-top-level.md sec 5 (landed 8cefda08). Files: src/habu/habu2.f + src/habu/aot-lib.f snapshot/AOT slices, test/top-row-snapshot-test.f. Acceptance: a snapshot/AOT image reproduces identical tier-1 warnings and tier-2 rejects for p1/p2/p3 as a cold source load (EFF-REC/ER.SYM persistence, now for full rows). Depends: tracker sub-dot.
