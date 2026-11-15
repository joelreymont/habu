---
title: Keep callback names of any length
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:46:08.928595+01:00"
closed-at: "2026-11-16T04:07:35.728912+01:00"
---

Problem: src/lib/ffi-callback.f copies tokens into a NAME-CAP (64) byte TOK-BUF (:240) and rejects longer ones with NAME-LONG (:287), a limit Habu set itself. Acceptance: a 100-byte callback name is kept whole; NAME-CAP and NAME-LONG are gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
