---
title: Unify the agent workspace protocol
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:04:23.242417+02:00"
---

AGENTS.md, CLAUDE.md, LLM.md, docs/parallel-agents.md, and docs/worker-briefing.md duplicate or contradict claim, workspace, publication, and worker authority rules. Define one repository-owned checked protocol record covering orchestrator and worker roles, isolated Jujutsu workspace creation, claim lifecycle, allowed mutations, review handoff, gate ownership, merge, push, and cleanup. Generate the consumer-facing instruction files from that source while preserving only tool-specific presentation differences. The current single-orchestrator local-claim dispatch rule must appear consistently; workers never infer authority from stale prose. Add a protocol conformance lint that detects contradictory actions, unknown paths/commands, duplicated normative clauses, and generated-view drift. Keep human sentences in generated docs and do not encode machine-specific absolute paths. Files: checked protocol schema/renderer/tests and generated instruction views. Verify byte-stable generation, worker/orchestrator scenario fixtures, path/host/dot lints, and full native gate.
