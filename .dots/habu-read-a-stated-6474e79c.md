---
title: Read a stated count before a loaded record
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T12:36:07.215765+01:00"
---

Problem: src/core/checker.f:14811 (ix MARK @ < IF EXIT) with COUNT-VALUE's read order: in-process VERIFY:SOURCE-BUF on an engine that already loaded 16 constant TWN lays a source's 32 constant TWN out by 16 (BUFFER review finding 5); the load rejects the redefinition, and tools/check.f in a fresh process never reaches it. Acceptance: the check lays out by the source's stated value; CUT's index-order rule holds for records below MARK. Depends: none. Claim: unassigned.
