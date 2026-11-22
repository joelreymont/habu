---
title: Say str for lsp tool words lsp-core calls
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T15:50:12.843560+01:00"
---

Problem: tools/lsp-core.f has no split-str line, so the lsp tool words it calls (lsp-text, lsp-symbols, lsp-rename, lsp-docs, lsp-workspace, lsp-defs, lsp-references, and lsp-core's own REPLY-ERROR, HELD-EDITED?) keep ^u8 n rows. Acceptance: those rows say str, lsp-core passes one str value, lsp-boundary passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-lspa
