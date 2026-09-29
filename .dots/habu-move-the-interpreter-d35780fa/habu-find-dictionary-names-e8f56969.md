---
title: Find dictionary names in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.629799+03:00"
---

Problem: dictionary lookup for the interpreter is assembly (`LFIND`/`LFINDUSED`) while `NDICT` (`src/compiler/native/dict.f:47-103`) already reads records with package visibility in Habu.
Acceptance: `src/habu/outer.f` `OUTER:FIND` with the `LFIND`/`LFINDUSED` rules (open package public/private, used packages with `E-USING-AMBIGUOUS`, qualified names, global-first order, hash index), built on `NDICT`'s readers where they agree; a differential test against `search-wl` over the whole booted dictionary.
Files: `src/habu/outer.f`, `test/outer-find.f`.
Verify: spark `bin/hb --load test/outer-find.f`.
Depends: none in the lane (I1 was folded into I9a, `habu-move-evaluate-and-9119f746`).
Route: Alder (shared: src/habu/outer.f, test/outer-find.f).
Ownership: krait (Intel lane).
Claim: unassigned.
