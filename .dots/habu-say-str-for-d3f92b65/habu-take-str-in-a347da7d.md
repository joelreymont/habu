---
title: Take str in STR=, SB-APPEND, SB$ and SOURCE-APPEND
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T16:44:49.676786+01:00"
closed-at: "2026-11-23T10:11:21.715800+01:00"
---

Problem: src/lib/string.f STR= SB-APPEND SB$ and src/lib/source.f SOURCE-APPEND-BYTES SOURCE-APPEND-QPATH spell ^u8 n because unmarked callers (tools/lsp-core.f, test policy and suite text) pass ^u8 n; SB-APPEND-LEN duplicates SB-APPEND. Acceptance: the rows say str, SB-APPEND-LEN is gone, every caller passes str, the whole suite passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p2strw
