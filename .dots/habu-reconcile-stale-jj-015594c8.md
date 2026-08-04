---
title: Reconcile stale jj workspaces
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T16:35:17.983816+02:00"
---

Why: merged and abandoned lanes were not consistently cleaned. The 2026-08-03 snapshot found 479 `.jj-ws` entries and 491 registered workspaces; the 2026-08-04 snapshot finds 481 and 493. Result: perform one safe census pass over every registered or on-disk workspace. For each workspace, record its tip, parent, owning dot, and commits not reachable from master. Forget the workspace and trash its directory only when every change is already on master or explicitly abandoned or superseded. Retain unique valid work and record its exact commit under the owning dot; do not merge product work in this cleanup. Remove missing-directory registrations only after recording their tips.

Current cleanup ledger: retain `.jj-ws/habu-checked-nominal-and-94be09c9`, `.jj-ws/habu-own-gpt-2-45d7d1e4`, `.jj-ws/habu-own-model-asset-c6f938e4`, and `.jj-ws/habu-speed-up-scalar-8f5563d2` until their work is merged or explicitly abandoned. Retain `/tmp/habu-sha256-bench.f`, `/tmp/habu-sha256-bench64.f`, `/tmp/habu-sha256-profile.f`, and `/tmp/habu-sha64.bin` only while the SHA lane needs them. At each successful landing or abandonment, immediately forget the workspace, trash its directory and lane-private temporary artifacts, and verify both the registry and default workspace are clean. Before each push or session handoff and after each feature wave, reconcile the registry, `.jj-ws/`, lane-private temporary artifacts, and this ledger. Record newly discovered cleanup obligations here instead of creating another cleanup tracker.

Acceptance: no safely removable stale workspace remains; every retained non-active workspace has an exact dot or unique-commit reason; no unique commit is lost; every completed lane has been removed from the registry and disk; the repository and default workspace are clean. Forbidden: bulk deletion, product code change, permanent cleanup script, report, framework, lint, manifest, doc, or suite.
