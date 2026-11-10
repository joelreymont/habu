---
title: REACHED-CK skips the stores of files checked but not yet linked
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T13:02:01.355881+02:00"
---

Problem: src/habu/linker.f REACHED-CK (about :2131) skips pending rows and STORERS holds only built pieces, so a checked-but-unlinked piece's stores into header data or into a reached piece's data are not required at the check but are at the link: the same gap habu-a-third-build-94cd6d9a closes for names, and its names table does not settle it. Reproducer (the lane's review, 2026-11-10; fixtures /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/atk): h.f `package ATK-H public variable HV : HGET ( -- n ) HV @ ; ;package`, p.f `42 ATK-H:HV !`, x.f `ATK-H:HGET drop` then `: ATK-X-WORD ( -- ) ;`, root-r.f requires h, p, x and alone-r.f requires h, x; building root-r, alone-r, root-r in one out root, the third fails E-STALE rc 74 with and without the lane (p.f defines no name, so the refusal is REACHED-CK's). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the sequence builds three times; a table of pending stores lets the check require what the link requires; a case in test/file-objects.f fails before. Files: src/habu/linker.f. Verify: the sequence from fresh out roots; test/file-objects.f. Depends: habu-the-pre-link-44b5d596 (same lane, the commit after it). Ownership: the file linker. Claim: agent=bob workspace=.jj-ws.noindex/bob-stale
