---
title: Offer no declaration a signature cannot spell
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.317829+01:00"
closed-at: "2026-11-16T04:07:35.259289+01:00"
---

Problem: src/core/render.f:1749-1751 END-HELP offers 'or declare ( ... )' with variables past the 23rd named by number, which a source signature rejects; test/engine-suite.f's '276 type variables: no warning' check repeats the caller check before it. Acceptance: the help leaves the clause out then, an engine test pins it, the redundant check is gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
