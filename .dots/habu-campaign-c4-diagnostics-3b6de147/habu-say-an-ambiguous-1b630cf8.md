---
title: Say an ambiguous family is ambiguous everywhere
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:02:55.688095+01:00"
closed-at: "2026-11-16T04:07:34.135734+01:00"
---

Problem: sum-type variant payloads (src/core/sumtype.f TFAM-SIG-RESOLVE calls ~:524/537/604/1286) report an ambiguous type family as unknown, and GENR-SIG-PARTS (src/core/check-walk.f) writes the ambiguous note and help under code E-GENERATES-ROW. Acceptance: each writes E-USING-AMBIGUOUS with the packages note and help. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
