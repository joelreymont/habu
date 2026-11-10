---
title: User source can call the record words on a sealed package
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T15:15:08.215155+02:00"
---

Problem: after habu-a-load-accepts-db95cbfb's first commit the sealed-package refusal sits where source names a definition (definers.f DEF-SEAL-GUARD, packages.f PKG-EXPORT); user source that calls OUTER:DEF-RECORD or OUTER:ALIAS-RECORD with a sealed package's wid is not refused: DEF-RECORD opens a definition there (load and check exit 74, 'source ended inside definition: A', naming a stale token) and ALIAS-RECORD writes a record (ndict@ grows by one; the name is then E-UNDEFINED, cause not found). Probes /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/seal-p2 (r-def.f, r-alias.f, r-alias2.f). Found by that commit's worker (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/seal2-done.md); its review rules on reach. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: user source cannot write a record into a sealed package by any word: the record words are not nameable from user source, or refuse; the three probes are refused with a line that names their own token; a test pins each. Files: src/habu/definers.f, src/habu/packages.f, test/seal.f. Verify: the probes under --load and tools/check.f. Depends: habu-a-load-accepts-db95cbfb. Ownership: package sealing. Claim: unassigned.
