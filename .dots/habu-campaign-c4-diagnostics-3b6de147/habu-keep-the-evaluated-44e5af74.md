---
title: Keep the evaluated token in a rejection
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.890079+01:00"
---

Problem: since EVALPL a record from evaluated text is placed at the invocation and loses the token it is about (src/core/rejection.f CLOSE-PART). Acceptance: the record carries an evaluated place (source_line, column, token) in JSON and a caret line in prose; lsp and GJA read it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-evalpl2.
