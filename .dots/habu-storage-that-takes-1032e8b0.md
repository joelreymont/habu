---
title: Storage that takes a hand-written width takes a type instead
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T19:08:12.444229+02:00"
---

Rule (Joel, 2026-11-11, his words): 'make sure we are using the same approach everywhere else possible. we should be dealing with types directly, not manually specifying the width of the types. because the width of the type might change, for example! create typed containers only!' and 'ask me if in doubt'. Recorded in docs/design.md 'Writing and changing Habu' and docs/forth-card.md section 4. Problem: Habu and its consumers size typed storage by hand in several forms (TASK:+USER takes a byte count: 'CELL TASK:+USER NAME', 'STORAGE-BYTES TASK:+USER STORAGE'; 'create R n cells allot' and 'create R n SCRATCH-ALLOT' for records; MEM:ALLOC-BYTES and ARENA:ALLOC callers sizing a record; the arrays design's width<t> value passed by a program). Acceptance: a census of every such form in src/, tools/ and the three consumers, as path:line with a count per form, each marked already typed, can take a type (with the typed form it would use) or cannot (why); then one dot per form that changes. The one way a program states a stored type is being designed first (dot habu-give-an-arena-25a7d3cc, brief .out/briefs/design-vector-states-type.md, which was widened to this rule); the census follows its report. TASK:+USER's second step (dot habu-user-band-cannot-4ca18d06: effect '( n -- )' taking bytes) is under this rule and is redesigned to take a type before its worker starts. Files: census only. Verify: the census file and the dots made from it. Depends: habu-give-an-arena-25a7d3cc design. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-census (census, brief .out/briefs/census-hand-written-widths.md).
