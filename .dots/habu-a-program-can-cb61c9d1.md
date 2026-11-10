---
title: A program can call prot-wid-add
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:53:31.148999+02:00"
---

Problem: prot-wid-add (src/habu/habu1.f :5238, src/habu/kernel-x64.f :4256) is a public engine word a program's source can call, so a program can mark any wordlist id below PROT-WID-MAX protected and make later definitions into it fail with E-SEALED-PACKAGE. Found by the hunt for habu-a-load-accepts-db95cbfb (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/seal-hunt.md, other findings; probe /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/seal/f/pw.f). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Related: habu-refuse-prot-wid-fb9b160a refuses it only while a checker overlay is open. Acceptance: a program's source cannot change which wordlists refuse definitions except by seal-package on a package it opened; when the record rule of db95cbfb covers the engine's own wordlists the word and its bitmap go, else it moves to a private section. Files: src/habu/habu1.f, src/habu/kernel-x64.f, src/habu/layout.f, test/seal.f. Verify: a program calling prot-wid-add is refused at that token under --load, tools/check.f and a build. Depends: habu-a-load-accepts-db95cbfb. Ownership: package sealing. Claim: unassigned.
