---
title: "Write primitives' destinations into a vector"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:23:37.263334+01:00"
---

Problem: read, readlink, realpath and getdirentries64 (src/habu/prims.f:456, :466, :467, :472) take a ^u8 n the word writes, a row shape Joel ruled out (2026-11-19: str or vector). Acceptance: each primitive's callers pass a ^VEC:vector<u8> grown by VEC:EXTEND, or the row takes the vector itself; no primitive row takes a destination ^u8 n. Depends: none. Claim: unassigned.
