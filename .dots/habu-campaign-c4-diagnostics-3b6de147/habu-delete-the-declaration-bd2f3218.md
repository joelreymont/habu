---
title: "Delete the declaration transaction's E-ACTIVE"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T14:37:41.551684+01:00"
closed-at: "2026-11-17T07:41:49.383039+01:00"
---

Problem: src/core/declaration-transaction.f OPEN (:16) throws E-ACTIVE (:10, 7166; EXPLAIN row generated-declaration-protection.f:9, sentence :60) when a declaration opens inside another. No front end runs a body token, live or replayed, so declarations never nest and no input or test reaches it; it also fires only after DECL-REJECT:OPEN and DRIVE's reset have overwritten the shared cells, so it guards nothing. Acceptance: the check, its code, its EXPLAIN row and the sentences naming it (generated-declaration.f ~:462, generated-declaration-protection.f:60) are gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-bd2.
