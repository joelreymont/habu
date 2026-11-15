---
title: Note the word of an evaluated TRUST record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.806268+01:00"
closed-at: "2026-11-16T04:07:35.270347+01:00"
---

Problem: in test/name-length-test.f the evaluated MAL$ SIG$ TRUST record shows no '= word: q:q:q' note, while the CHECKER-DEFER and DEFCAST records evaluated the same way show one; and the file's head and note pins include the three-digit gutter, so they break when MAIN leaves lines 100-999. Acceptance: the TRUST record carries its word note as the others do; the pins match without the gutter width. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
