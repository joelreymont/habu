---
title: Checked source can read through NULL-PTR and crash the engine
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:24:27.410741+02:00"
---

Problem: ': T ( -- n ) NULL-PTR BYTE-VIEW c@ ;' passes the checker and 'T drop' ends hb with a segmentation fault (rc 139), under --load and under tools/check.f, which crashes with it. Reproduced by the lead on master 951b3f90. 'XREF-NULL XREF-INLINE-NAME c@' does the same (the word answers 0x18 without reading). src/habu/prims.f :1003-1011 says a value read through NULL-PTR is refused; this route is not. Found by the review of habu-xref-sealed-on-b27a8e57 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/null-review.md, Outside this commit). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which reads through a null the checker refuses and which it admits, why BYTE-VIEW (and any word like it) loses the null kind, and what a refusal or a guard would cost. Acceptance: no checked program reads memory through NULL-PTR and crashes the engine: it is refused by the checker with a reason, or stops with a catchable code. Files: named by the hunt. Verify: the two reproducers, refused or caught. Depends: none. Ownership: reads through the null pointer. Claim: agent=bob workspace=.jj-ws.noindex/habu-checked-src-can-88e8a977
