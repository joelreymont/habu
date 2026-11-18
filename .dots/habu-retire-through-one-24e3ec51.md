---
title: Retire through one word on every machine
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:04:09.612819+01:00"
blocks:
  - habu-a-caught-failed-9bb3e339
---

Problem: after habu-a-caught-failed-9bb3e339 records are retired by a private XREF word in Habu and, for CHECKER-OVERLAY, GENERATED-DECL-DICTIONARY and LINKER (src/habu/linker.f FORGET-SPACE), by the kernel primitive retire-from (src/habu/habu1.f BRETIREFROM over EM-DEF-OCC:LRETIRE; src/habu/kernel-x64.f RETIRE-FROM-BODY): two implementations of one thing. Acceptance: those three callers retire through the XREF word while a task is live; the retire-from rows (src/habu/prims.f) and both kernel bodies are gone. Depends: habu-a-caught-failed-9bb3e339. Claim: unassigned.
