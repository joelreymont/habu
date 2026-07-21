---
title: Validate every normative source path
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T22:04:23.251152+02:00"
---

Normative docs currently cite nonexistent source owners such as src/config.fs, src/prims.fs, src/control.fs, and src/pickroll.fs, while host settings contain absolute machine-specific commands. Extend a checked documentation/operations path lint to parse only declared normative references, resolve repository-relative file and optional line anchors against the exact tree, and reject missing, moved, ambiguous, absolute-host, or out-of-workspace paths. Provide an explicit schema for generated artifacts and external references so they cannot be mistaken for source paths. Apply it to coding standards, effects documentation, agent instructions, skills, and command allowlists. Replace host-specific settings with repository-relative, argument-aware policy generated from one source. Add rename/delete/line-drift/absolute-path/escaped-path fixtures and prove every current normative reference is covered. Files: path policy/lint/tests and corrected docs/settings. Verify host/filemap/dot lints and full native gate.
