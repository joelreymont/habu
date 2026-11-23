---
title: "Write primitives' destinations into a vector"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:23:37.263334+01:00"
---

Problem: read, readlink, realpath, getdirentries64 and does-patch (src/habu/prims.f:431, :441, :442, :448, :519) take a ^u8 n the word writes, a row shape Joel ruled out (2026-11-19: str or vector). Acceptance: each primitive's callers pass a ^VEC:vector<u8> through a library word that grows it (dot habu-grow-a-vector-a8d202dd) or the row takes the vector itself; no primitive row takes a destination ^u8 n. Depends: habu-grow-a-vector-a8d202dd. Claim: unassigned.
