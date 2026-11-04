---
title: Rename the E-NCOMP- error codes to E-COMP-
status: closed
priority: 2
issue-type: task
created-at: "2026-11-03T12:52:37.667906+02:00"
closed-at: "2026-11-03T13:14:40.236619+02:00"
close-reason: "Landed master 09565d43 'Rename the E-NCOMP- codes to E-COMP-': 8 codes in src/lib/errors.f, same numbers, every use in 30 files under src, tools, test, docs; dots and LESSONS.md keep the old name as history. Gates on the candidate (.jj-ws.noindex/jerry-ecomp/.out/cand/hb, sha256 522e062b…): build rc 0, x86-64 cross-build rc 0, full suite rc 0 (518 of 518). rg E-NCOMP- over src tools test docs finds nothing at the landed tip. Install request sent to carl."
---

Joel 2026-11-03 (LESSONS "T0's callback request and shadow cell"): the 8 global E-NCOMP- codes in src/lib/errors.f (STATE -8570, TEXT, VERDICT, NAME, OWNER, BINDING, RECOVERY, ARITY) become E-COMP- with the same numbers, every use in src, tools, test and docs renamed; NCOMP-DISPATCH and NDEFINITION keep their names. Mechanical; before lane 2, which refuses with E-COMP-STATE. Brief /tmp/jerry-big/ecomp-brief.md. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ecomp.
