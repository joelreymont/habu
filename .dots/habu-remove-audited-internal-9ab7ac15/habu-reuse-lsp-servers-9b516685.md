---
title: Reuse LSP servers across ordinary cases
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:42.014575+03:00"
closed-at: "2026-10-22T12:36:35.227087+03:00"
close-reason: "Triage 2026-10-22, DONE: tools/lsp-test-lib.f SESSION-RUN/SESSION-TURNS :7390-7413 serve compatible cases from one initialized child (SHARED?), added in 3bdffeeafa17."
---

tools/lsp-test-lib TALK always spawns and shuts down server including ordinary completion/definition cases. Share server for suites whose immutable initialization/process state allows it; isolate documents/fixtures. Keep separate servers only startup/shutdown/framing failure/capability or workspace configuration boundaries with reason beside case. Preserve existing complete LSP test behavior and process cleanup; independent review.
