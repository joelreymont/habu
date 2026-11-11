---
title: MATCH over a logical cell resolved later is refused as unresolved
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:02:13.558451+02:00"
---

Problem (third review of carl-depth d7731548; probes ~/.cache/tmp/rv-depth3/p/ m9.f, m8.f, m0.f; master behaviour, the depth commit does not touch MATCH): ': CS-D9N ( -- option<a> option<a> ) OPTION:NONE OPTION:NONE ; : CS-D9E ( option<n> -- ) drop ;' then 'CS-D9N CS-D9E MATCH option none OF 0 ENDOF some OF ENDOF ;MATCH' is refused rc 70 'bad match: scrutinee type arguments are unresolved' (E-MATCH-OPEN-ARGS), although the cell's arguments resolved to n; binding it to a local and pushing it back (m8.f) is admitted and prints 0, a concrete option (m0.f) prints 3. Likely cause, not proven: the MATCH scrutinee path (checker.f MATCH-SCRUT-DIAG near 22655, HIDDEN-PARAM? / MD-OPEN-ARGS) does not expand a resolved logical cell as LOC-BIND and CWIN-LIVE do. Acceptance: m9.f admitted and prints 0; a scrutinee whose arguments stay open is still refused E-MATCH-OPEN-ARGS. Files: src/core/checker.f, test/checker-soundness-suite.f. Verify: m9.f rc 0; a soundness case that fails before the fix. Depends: none. Ownership: checker.f MATCH scrutinee typing. Claim: unassigned.
