---
title: "Place a storage count's record only from input that holds it"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T11:36:00.927074+01:00"
closed-at: "2026-11-16T04:07:35.026821+01:00"
---

Problem: src/core/layout-buffer.f:49 LBUF-COUNT-TOKEN guesses a run-time bad count's place as the token two before the definer's name; through `execute` or `catch` (`-1 ' TYPED-BUFFER execute R4`) it marks `TYPED-BUFFER` or a handler's name as the count and its help says to replace that. Acceptance: a count rejected at run time is placed at the buffer's name, which the definer always read from the input, with the count's value in the message and a help that names no token as the count; LBUF-COUNT-TOKEN is gone; the static path (checker.f COUNT-OK?), which reads the count's own token, is unchanged. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
