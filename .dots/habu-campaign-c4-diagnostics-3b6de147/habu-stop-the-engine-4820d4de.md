---
title: "Stop the engine's rejections with status 70"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.101021+01:00"
closed-at: "2026-11-16T04:07:33.933331+01:00"
---

Problem: src/habu/interpreter.f, verify-source.f:1042 and checker.f:12497/:12513 stop or die after a written record with an ENGINE-ERROR status (`;using` alone exits 93, a definition in a sealed package 84). Acceptance: every such record ends with 70 under the three readers; constants left unused are deleted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
