---
title: Hint recurse only where recurse calls the word
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:49:48.076148+01:00"
closed-at: "2026-11-16T04:07:35.825019+01:00"
---

Problem: render.f UNDEF-HELP gives the recurse help for `['] FACT` and `[: FACT ;]` inside FACT, where it is wrong: a tick of recurse gives no xt, and recurse inside a quotation calls the quotation; no test pins the recurse help. Acceptance: a tick or quotation gets the define help; a test pins the recurse help and these two. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
