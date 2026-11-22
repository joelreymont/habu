---
title: "Record a names: literal across >str in bodies"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T17:00:09.291725+01:00"
---

Problem: the checker body walk arms a names: literal only for the token right after it (src/core/check-walk.f SCAN-TOK-HEAD, NAMED-OPERAND-USE), so unmarked s" X" >str W records no use; lsp-boundary references-named fails. Acceptance: one engine >str keeps the literal armed; lsp-boundary passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-land
