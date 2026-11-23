---
title: Grow the fixed primitive, claim and fault tables
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T18:45:09.554303+01:00"
---

Problem: src/habu/prims.f NAMES, CODES and ROWS, data-claims.f NAMES and fault-record.f FAULT-PROSE/FAULT-JSON are fixed create tables whose overflow is an internal die; a new row overflows them (census .out/census/dies-c.md, class GROW). Acceptance: each is storage that grows (as ENGINE-PRIMS's DYNAMIC-BUFFER), with no capacity die; engine builds converge. Depends: none. Claim: unassigned.
