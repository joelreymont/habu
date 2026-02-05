---
title: Enforce commit+dot flow
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-05T12:11:06.398562+01:00\\\"\""
closed-at: "2026-02-05T12:35:39.908494+01:00"
close-reason: Added tools/dot-finish + documented workflow.
---

AGENTS.md: add mandatory 'use tools/dot-finish' rule. tools/dot-finish: run zig build test; ensure jj status clean/empty change described; jj describe -m; jj git push; dot off <id>. Root cause: manual dot off can skip commit/push. Fix: single command wrapper used for every dot closure.
