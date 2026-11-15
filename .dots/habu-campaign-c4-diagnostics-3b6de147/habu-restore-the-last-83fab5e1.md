---
title: Restore the last token when a buffer throws
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:07:57.088684+01:00"
closed-at: "2026-11-16T04:07:34.165233+01:00"
---

Problem: throw recovery (src/habu/habu2.f EM-EVAL-THROW-RECOVER, src/habu/interpreter.f FRAME-RECOVER and REPL-RECOVER) puts back the input but not the token, so src/core/include.f LOAD-CURRENT keeps its own save and a throw out of RUN-PROGRAM leaves the token in released bytes. Acceptance: after a caught throw out of a load or an evaluated string the token is the outer word; LOAD-CURRENT's save is gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
