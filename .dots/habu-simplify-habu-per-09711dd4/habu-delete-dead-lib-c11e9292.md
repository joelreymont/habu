---
title: Delete dead library words and split C2-MEM
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.034956+03:00"
---

Problem: public words nothing calls (no caller in Habu's src, tools or test, nor in Zen, Tender or Maki): src/lib/build.f CHECK, ARTIFACT, STEP and RUN; src/lib/ffi-abi.f FLOATS, STACK and KPARAMS>CELL, and its private FFI-X8!; src/lib/test/record.f TREC-N.. Package C2-MEM is opened in three files: src/lib/c2-memory.f, src/lib/c2-memory/owner-runtime.f (which c2-memory.f includes) and src/lib/c2-owner.f (which seals it). Acceptance: each of these words is deleted, and C2-MEM is one file. Depends: none. Claim: unassigned.
