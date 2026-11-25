---
title: Tail-call kernel words from T0 code
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T19:13:21.513685+01:00"
---

Problem: src/habu/t0.f (TAIL at the line that tests NPUB:IN-REGION?) makes a call in tail position a jump only when the callee is in the code region, for the tail-call check and call map that the site-map deletion removes; a kernel word's call there keeps call + return (5 bytes + ret on x86-64, frame save on arm64). Acceptance: a tail call of a kernel word that needs no return address of its own (census: catch, throw, debug and profiler words, words that read the caller's return address) is a jump on both targets; code-bench is no slower and the engine and app binaries shrink; measured sizes in the commit. Depends: habu-find-whether-anything-b457bf9c. Claim: unassigned.
