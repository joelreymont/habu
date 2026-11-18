---
title: Enforce an inferred any-stack quotation input
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:16:26.509130+01:00"
---

Problem: ': T6 if drop [: ;] then [: ;] swap execute execute ;' infers ( [ -- ] bool -- ) yet ': U6 ( -- ) 5 [: drop 7 ;] false T6 . ;' is certified and dies with a bus error (rc 135); the literal form and a call through ': PICK2 ( a a bool -- a )' die too. And ': RUN2 ( R [ R -- S ] -- S ) execute ;' rejects '[: ;] dup RUN2', which is sound. One cause: a pairing treats a literal's RVK-INFERRED tail alike on both sides; src/core/checker.f HOLD-TAIL holds it instantiable when demanded, and QAPP-FRESH? in pair mode binds a provided one in place instead of copying it. Acceptance: U6, the literal and PICK2 forms are rejected; '[: ;] dup RUN2 drop' is accepted and runs; a test pins each and fails first. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-anyq.
