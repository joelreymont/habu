---
title: Kernel stops write records
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:23:42.220635+01:00"
closed-at: "2026-11-16T04:07:35.684265+01:00"
---

Problem: four arm64 kernel stops write a hand-made line or their own record and end with a status a program cannot catch as a code: LFLOORREC (E-UNDERFLOW, 70), LDPBAD (E-DATA-SPACE, 76), OVERLAY-EMIT:LDIE (E-OVERLAY-OPEN, 108) and LBGROWFAIL (E-BODY-TEXT, 71) in src/habu/habu2.f. Acceptance: each throws a named error.f code; caught, it is silent; uncaught, it gets the uncaught-throw record (habu-place-an-uncaught-d11d0078). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
