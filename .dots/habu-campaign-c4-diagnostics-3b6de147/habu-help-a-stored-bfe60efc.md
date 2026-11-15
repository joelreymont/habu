---
title: Help a stored row and pin an ambiguous local
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T22:08:58.135983+01:00"
closed-at: "2026-11-16T04:07:35.887953+01:00"
---

Problem: re-review 7a. (1) checker.f:11351 SGBAD-HELP hands SGBAD-TOKEN (the declaring name when the verdict's token is the whole row) to SGBAD-FIX as the rejected token, so a stored row's help says 'give `T5` 1 type argument'; reproducer was a trust row, which master deleted: check which stored rows still reach it. (2) No test pins a local's ambiguous record (render.f:1959 DIAG-ADVICE): {: x:es-amb<n> :} after test/engine-suite.f's ES-AMB setup. Acceptance: a stored row's help names the rejected type; a test pins the ambiguous local's message and help. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
