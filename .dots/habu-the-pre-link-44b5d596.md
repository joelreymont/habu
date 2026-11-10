---
title: The pre-link check misses names an unlinked file changes
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T13:02:01.343431+02:00"
---

Problem: src/habu/linker.f LINK-PRIVATE, LINK-SEALED, LINK-RETIRED and LINK-EARLIER call CHANGER+ at link, so the link indexes the names a piece changes as well as those it defines; the pending-names table of habu-a-third-build-94cd6d9a holds only defined records, so the check can pass a file whose source spells a name a checked-but-unlinked piece changes, and the link then refuses it (E-STALE). Reproducer (the lane's review, 2026-11-10; fixtures /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/atk, seq.sh runs a sequence): ps.f `package ATK-S public variable SV ;package`, ds.f `package ATK-S seal-package ;package`, ss.f `: ATK-SS-WORD ( -- ) ; ( ATK-S )`, root-s.f requires ps, ds, ss and alone-s.f requires ps, ss; building root-s, alone-s, root-s in one out root, the third fails `threw -9692 LINKER:E-STALE rc=74` with and without the lane. Cause: PENDING-NAMES+ skips KIND-SEALED and KIND-PRIVATE entries. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the sequence builds three times; the check fails the spelling file so it compiles; the names a pending piece changes are records of its row, kept out of the record indices PENDING-KEY relies on; a case in test/file-objects.f fails before. Files: src/habu/linker.f. Verify: the sequence from fresh out roots; test/file-objects.f. Depends: none; a second commit on the lane of habu-a-third-build-94cd6d9a, after its review fold-in. Ownership: the file linker. Claim: unassigned.
