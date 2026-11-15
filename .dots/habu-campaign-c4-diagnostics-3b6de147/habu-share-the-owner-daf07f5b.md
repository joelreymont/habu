---
title: "Share the owner record's close lookup"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.372561+01:00"
closed-at: "2026-11-16T04:07:34.067650+01:00"
---

Problem: src/habu/interpreter.f:2417-2423 DEF-BODY-CLOSER copies src/core/include.f:2220-2229 BODY-CLOSE's lookup, with a 'predates the slot' size test no record can fail. Acceptance: one CHECKER-OWNER-ABI word answers the close action, without the size test, for both callers. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
