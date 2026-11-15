---
title: "Place a type's records at the token they judge"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T03:07:09.231517+01:00"
closed-at: "2026-11-16T04:07:35.016786+01:00"
---

Problem: src/core/structure-decl.f FIELD-FAM?, SD-PARSE-NODE-IMPL and SD-LOOSE-ATOM reject without placing the token they judge, so DEFTYPE dx option<width> underlines option<width>, not width. Acceptance: each such record stands under the inner token, the same from the load and check.f, pinned in deftype-suite. Depends: none. Claim: agent=W168 workspace=.jj-ws.noindex/jerry-w168.
