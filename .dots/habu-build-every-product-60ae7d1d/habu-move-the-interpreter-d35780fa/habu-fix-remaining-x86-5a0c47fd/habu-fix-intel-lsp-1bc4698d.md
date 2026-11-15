---
title: Fix Intel LSP lifecycle timeout
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T13:54:40.545921+00:00"
closed-at: "2026-11-04T11:48:48.126725+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. 25 s x86 LSP load through the tier-1 Habu loop on intel; CONVERSATION-MS guard finding (lsp-test-lib.f:620) is separate KEEP; workspace not listed."
---

Owner: pulsar /root/lsp_rows in .jj-ws.noindex/pulsar-intel-lsp. Combined candidate census reaches the lifecycle conversation but produces no stdout or stderr before the unchanged 8 s read deadline. Trace actual staged server path and fix the responsible layer; preserve master test expectations and timeouts. Acceptance: existing tools/lsp-test.f and tools/lsp-entry-test.f green plus independent review for a significant fix. Shared changes require an OPEN Mac ARM check.
- Lead note (jerry, 2026-10-23): CONVERSATION-MS (tools/lsp-test-lib.f:620, 8000) is a deadlock guard whose own comment says a slow server must never reach it, so it is mis-specified for a 25 s x86 LSP source load; fix it at its layer (row budgets, or a guard on progress) as a shared change with an ARM check, and treat the x86 25.1 s load (ARM tier 0 0.66 s, ARM tier 1 via the Habu loop 14.8 s) as its own defect.
