---
title: Reconcile stale jj workspaces
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T16:35:17.983816+02:00"
---

Why: .jj-ws currently contains 479 directories and jj lists 491 workspaces because merged and abandoned lanes were not consistently cleaned. Result: perform one safe census pass over every registered or on-disk workspace. For each workspace, record its tip, parent, owning dot, and commits not reachable from master. Forget the workspace and trash its directory only when every change is already on master or explicitly abandoned or superseded. Retain unique valid work and record its exact commit under the owning dot; do not merge product work in this cleanup. Remove missing-directory registrations only after recording their tips. The active GPT-2 CLI workspace is excluded until its merge. Acceptance: no safely removable stale workspace remains; every retained non-active workspace has an exact dot or unique-commit reason; no unique commit is lost; the repository and workspace registry are clean. Forbidden: bulk deletion, product code change, permanent cleanup script, report, framework, lint, manifest, doc, or suite.
