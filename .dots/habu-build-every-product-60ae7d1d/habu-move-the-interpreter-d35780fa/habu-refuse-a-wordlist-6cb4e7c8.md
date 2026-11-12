---
title: Choose the bound a wordlist id may not pass
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T04:36:12.521118+02:00"
---

Problem: wordlist ids are a dense counter (src/habu/layout.f:222 WIDN-CELL), and nothing that hands one out checks a bound: arm64 src/habu/habu1.f:4052 BWORDLIST, src/habu/habu2.f:1188 NAMESPACE-RECORD (the counter at :1209-1214 and :1236-1237) and habu2.f:1408 REPLAY-PRIVATE (:1428-1429); x86-64 src/habu/kernel-x64.f:4194 WORDLIST-ROWS, and :5068 NAMESPACE-RECORD-BODY. Which bound to refuse at is a choice nobody has made. The candidates: PROT-WID-MAX (layout.f:503, 8192), past which a package has no protection bit and so can never be sealed (habu1.f:4478 refuses its policy); the u32 wid of the compact AOT dictionary record (layout.f:551), past which a captured id would not fit; and the cell itself, whose wrap would reach the negative retired markers (WL:RETIRED and its kin). This is an acceptance case of its parent: fix it in the code that stays, never in the machinery the parent replaces.

Acceptance: the bound is decided, recorded beside layout.f's wid constants, and an implementation dot is filed: each allocator above refuses an id past it with a named engine error, on both arches.

Depends: none.

Claim: unassigned.
