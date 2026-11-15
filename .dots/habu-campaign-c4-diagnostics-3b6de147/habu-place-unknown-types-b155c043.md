---
title: Place unknown types at their token
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T03:11:34.742095+01:00"
closed-at: "2026-11-16T04:07:34.932959+01:00"
---

Problem: src/core/structure-decl.f LETTER-TYPE, SD-POINTEE-AT, SD-LOOSE-ATOM, SD-LOOSE-NODE-IMPL and SD-PARSE-NODE-IMPL reject an unknown type (and an out-of-arity parameter) without placing its token, so DEFTYPE dx option<nosuch> underlines option<nosuch>. Acceptance: each record stands under the token it judges, the same from the load and check.f, pinned in deftype-suite and structure-decl-suite. Depends: none. Claim: agent=W170 workspace=.jj-ws.noindex/jerry-w170.
