---
title: Rename the E-NCOMP- error codes to E-COMP-
status: active
priority: 2
issue-type: task
created-at: "2026-11-03T12:52:37.667906+02:00"
---

Joel 2026-11-03 (LESSONS "T0's callback request and shadow cell"): the 8 global E-NCOMP- codes in src/lib/errors.f (STATE -8570, TEXT, VERDICT, NAME, OWNER, BINDING, RECOVERY, ARITY) become E-COMP- with the same numbers, every use in src, tools, test and docs renamed; NCOMP-DISPATCH and NDEFINITION keep their names. Mechanical; before lane 2, which refuses with E-COMP-STATE. Brief /tmp/jerry-big/ecomp-brief.md. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ecomp.
