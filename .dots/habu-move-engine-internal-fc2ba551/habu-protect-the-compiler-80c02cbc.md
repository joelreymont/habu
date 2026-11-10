---
title: Protect the compiler dispatch owner cells
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:28:26.227613+02:00"
closed-at: "2026-11-10T21:50:20.748631+02:00"
close-reason: merged into habu-move-engine-internal-fc2ba551
---

Problem: checked code can write the dispatch owner pointer and its record: ': P10 ( ptr u8 -- ) NCOMP-DISPATCH:DECL-A ! ;' and ': P13 ( -- ) 0 NCOMP-DISPATCH:DECL-A @ c! ;' are accepted on master (layout.f DECL-A, TARGET-DECL-A are public RESERVED-PTR-U8-CELL), so any program can redirect the compiler's declaration owner. Registry-protection class (docs/design.md Registry protection). Acceptance: both programs refused with a code; owner writes still compile. Files: src/habu/layout.f and the DECL-A writers. Verify: rejected programs through check.f; engine build. Depends: none. Ownership: TBD by the lane. Claim: unassigned.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.
