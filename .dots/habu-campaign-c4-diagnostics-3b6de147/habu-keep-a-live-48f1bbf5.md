---
title: "Keep a live signature rejection across EFFECT:NAMED"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:02:55.718081+01:00"
closed-at: "2026-11-16T04:07:35.757382+01:00"
---

Problem: NAMED-FAMILY (src/core/check-hook.f) clears SGBAD before it resolves, wiping any SGBAD verdict still live when EFFECT:NAMED runs. Acceptance: a reproducer where a live SGBAD is lost, fixed; or a reading that shows none can be live there, with the clear's reason in its comment. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
