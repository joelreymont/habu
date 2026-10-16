---
title: Delete the replaced bootstrap signature file
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.865323+03:00"
---

bootstrap/src/sig.fs is unreferenced beside live render/sigparse/db/prims modules. Delete sig.fs and stale unify.fs reference. Confirm bootstrap/habu-lib.fs load graph and existing bootstrap signature behavior; do not delete live bootstrap checker.
