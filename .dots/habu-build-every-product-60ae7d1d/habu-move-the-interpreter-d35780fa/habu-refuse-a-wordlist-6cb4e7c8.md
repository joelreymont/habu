---
title: Choose the bound a wordlist id may not pass
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T04:36:12.521118+02:00"
---

Problem: wordlist ids are a dense counter (src/habu/layout.f WIDN-CELL, :200), and nothing that hands one out checks a bound: arm64 src/habu/habu1.f BWORDLIST (:3522), src/habu/habu2.f NAMESPACE-RECORD (:1020; the counter at :1151-1156 and :1179-1180) and habu2.f REPLAY-PRIVATE (:1248; the counter at :1378-1379); x86-64 src/habu/kernel-x64.f:3676 WORDLIST-ROWS, and :4438 NAMESPACE-RECORD-BODY. Which bound to refuse at is a choice nobody has made. The candidates: PROT-WID-MAX (layout.f:426, 8192), past which a package has no protection bit and so can never be sealed (habu1.f EMIT-PROTWIDADD, :4590, refuses such a wid); and the cell itself, whose wrap would reach the negative retired markers (WL:RETIRED and its kin). This is an acceptance case of its parent: fix it in the code that stays, never in the machinery the parent replaces.

Acceptance: the bound is decided, recorded beside layout.f's wid constants, and an implementation dot is filed: each allocator above refuses an id past it with a named engine error, on both arches.

Depends: none.

Claim: unassigned.
