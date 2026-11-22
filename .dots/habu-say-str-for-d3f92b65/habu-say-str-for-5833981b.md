---
title: Say str for library words lsp-core calls
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T15:50:12.854250+01:00"
---

Problem: tools/lsp-core.f and test/type-decl-suite.f have no split-str line, so JSON-RPC ID$ METHOD? >MESSAGE ERROR, URI:FILE>PATH, SPAN:$ COPY, REASON:TEXT$, FD:WRITE-FULL and SOURCE-ROOT:CANONICAL keep ^u8 n rows. Acceptance: those rows say str, the callers pass one str value, the engine builds and the suites that load them pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-lspb
