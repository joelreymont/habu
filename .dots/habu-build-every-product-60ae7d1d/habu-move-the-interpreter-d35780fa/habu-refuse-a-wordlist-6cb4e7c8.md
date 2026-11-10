---
title: Choose the bound a wordlist id may not pass
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T04:36:12.521118+02:00"
---

Problem: wordlist ids are a dense counter (layout.f WIDN-CELL), and nothing that hands one out checks a bound: arm64 habu1.f:4391 BWORDLIST, habu2.f:4180 NAMESPACE-RECORD (the counter at :4201-4206 and :4228-4229) and habu2.f:10015 C-PACKAGE-ALLOC-WIDS; x86-64 kernel-x64.f:4250 (the `wordlist` row) and :5122 NAMESPACE-RECORD-BODY. The layout's WID:MAX and the generated-declaration preflight's private WID-MAX, which the old text named, are gone. Which bound to refuse at is a choice nobody has made. The candidates in the tree: PROT-WID-MAX (layout.f:709, 8192), past which a package has no protection bit and so can never be sealed (outer.f:530 refuses its policy, habu1.f:5636 answers "never protected"); the u32 wid of the compact AOT dictionary record (layout.f:755), past which a captured id would not fit; and the cell itself, whose wrap would reach the negative retired markers (DICT-WL:RETIRED and its kin). Found reading the code. This is an acceptance case of its parent: fix it in the code that stays, never in the machinery the parent replaces.

Acceptance: the bound is decided, recorded beside layout.f's wid constants, and an implementation dot is filed: each allocator above refuses an id past it with a named engine error, on both arches.

Files: src/habu/layout.f; later src/habu/habu1.f, src/habu/habu2.f, src/habu/kernel-x64.f.

Verify: the decision is in layout.f and the implementation dot names the bound.

Depends: none.

Ownership: unassigned. Claim: unassigned.
