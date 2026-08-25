---
title: Reconcile stale jj workspaces
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T16:35:17.983816+02:00"
---

Claim: unassigned

Why: merged and abandoned lanes were not consistently cleaned. Periodically reconcile the live jj registry, `.jj-ws/`, and lane-private temporary artifacts.

Current holds: `.jj-ws/habu-speed-up-scalar-8f5563d2@a4cd46f805f6`; `/tmp/habu-sha256-bench.f`; `/tmp/habu-sha256-bench64.f`; `/tmp/habu-sha256-profile.f`; `/tmp/habu-sha64.bin`.

Before removal, establish the workspace tip, parent, owning dot, and commits not reachable from master. Forget and recoverably trash it only when its work landed or was explicitly abandoned or superseded. Preserve unique work and record its commit under the owning dot. After every landing or abandonment, remove its workspace and temporary artifacts immediately, then leave the default workspace clean. The live registry is the ordinary workspace inventory; do not copy it here.

Acceptance: lose no unique commit; remove every proved-completed lane from registry and disk; keep the repository and default workspace clean. Forbidden: bulk deletion, product code change, permanent cleanup script, report, framework, lint, manifest, doc, or suite.
