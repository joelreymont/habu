---
title: Fix Intel LSP lifecycle timeout
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T13:54:40.545921+00:00"
---

Owner: pulsar /root/lsp_rows in .jj-ws.noindex/pulsar-intel-lsp. Combined candidate census reaches the lifecycle conversation but produces no stdout or stderr before the unchanged 8 s read deadline. Trace actual staged server path and fix the responsible layer; preserve master test expectations and timeouts. Acceptance: existing tools/lsp-test.f and tools/lsp-entry-test.f green plus independent review for a significant fix. Shared changes require an OPEN Mac ARM check.
