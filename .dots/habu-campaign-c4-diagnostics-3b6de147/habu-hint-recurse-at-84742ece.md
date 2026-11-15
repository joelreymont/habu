---
title: Hint recurse at every undefined self-call
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:34:21.148781+01:00"
closed-at: "2026-11-16T04:07:35.838813+01:00"
---

Problem: interpreter.f UNDEFINED-NAME (:311) and t0.f NOT-FOUND (:196) print 'define X before this point' with no recurse hint when X is the word being defined; render.f UNDEF-HELP now gives it. Acceptance: each writer a self-call reaches gives the recurse help, or is shown unreachable for one and left as is. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
