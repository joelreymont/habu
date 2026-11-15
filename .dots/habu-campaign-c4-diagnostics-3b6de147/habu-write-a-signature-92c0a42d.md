---
title: "Write a signature rejection's help in one place"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:35:23.676015+01:00"
closed-at: "2026-11-16T04:07:33.786408+01:00"
---

Problem: src/core/render.f SIG-HELP and src/core/checker.f SGBAD-HELP$ each word the help of one signature verdict their own way, so a stored signature writing `ptr` is not told ^T; a local's annotation is reported as 'in signature' and told apart by the rejected text's length. Acceptance: one help writer per verdict kind serves both records; a local's record names the local; tests pin both. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
