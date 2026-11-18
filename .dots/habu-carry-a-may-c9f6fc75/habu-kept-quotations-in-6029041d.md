---
title: Kept quotations in tools and tests state their facts
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T09:25:03.309493+01:00"
closed-at: "2026-11-18T09:50:36.310473+01:00"
---

Problem: with the c9 checker, lsp-test-lib.f SESSION-RUN and kept quotations in signature-nesting, prop, native-eval, ffi-callback, tasking-threads and checker-completion tests are rejected. Acceptance: those suite entries pass with the c9 engine. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c9s2 (sites 1, 3, 7), .jj-ws.noindex/jerry-c9t2 (task and callback tests), .jj-ws.noindex/jerry-c9e1 (signature-nesting).
