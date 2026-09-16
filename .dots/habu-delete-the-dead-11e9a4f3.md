---
title: Delete the dead words and payload the audit found
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:25:10.800315+03:00"
---

Problem: an Opus audit (2026-09-16, scratchpad audit-bloat-core.md cuts 5 and 6, the wrej note) found 25 unreachable definitions in src/core/checker.f (about 54 lines, among them SIGSCOPE!/SIGSCOPE-OFF, whose absence makes the IF arm of SIG-SCOPE$ near line 3446 a permanently false branch because SIGSCOPE-ON is set only inside a dead word), the dead PFX-APPEND-ENGINE-SNAP-HOOK chain in src/habu/habu2.f (lines about 307, 310, 778-795, 9493) whose 61-byte payload at about 2012-2014 is baked into every engine and never read, and the wrej refuse leg (habu2.f about 6167-6182, 16 lines of ARM64 baked into every engine) that its own comment says has no producer. Acceptance: every listed definition, branch and payload is deleted with its callers reconciled (a permanently false branch becomes straight-line code); the tree-wide dead-word tool (habu-census-and-delete-fe9a93f3) confirms zero references before each deletion; engine bytes before and after; byte fixpoint; stage0 chain OK where habu2.f emission changes; full gate green. Files: src/core/checker.f, src/habu/habu2.f, bootstrap/cg/forth.fs if the mirror carries the payload. Verify: rg for each name; tools/native-build.f fixpoint; tools/bootstrap.sh; test/run.f. Depends: none. Ownership: checker and engine emitter. Claim: unassigned.
