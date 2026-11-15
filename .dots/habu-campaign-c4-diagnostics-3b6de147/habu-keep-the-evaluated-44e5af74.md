---
title: Keep the evaluated token in a rejection
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.890079+01:00"
closed-at: "2026-11-16T04:07:35.694197+01:00"
---

Problem: since EVALPL a record from evaluated text is placed at the invocation and loses the token it is about (src/core/rejection.f CLOSE-PART). Acceptance: the record carries an evaluated place (source_line, column, token) in JSON and a caret line in prose; lsp and GJA read it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
