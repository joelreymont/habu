---
title: Reuse LSP servers across ordinary cases
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:42.014575+03:00"
---

tools/lsp-test-lib TALK always spawns and shuts down server including ordinary completion/definition cases. Share server for suites whose immutable initialization/process state allows it; isolate documents/fixtures. Keep separate servers only startup/shutdown/framing failure/capability or workspace configuration boundaries with reason beside case. Preserve existing complete LSP test behavior and process cleanup; independent review.
