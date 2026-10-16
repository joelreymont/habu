---
title: Delete the replaced bootstrap signature file
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.865323+03:00"
closed-at: "2026-10-16T11:54:59.851324+03:00"
close-reason: Deleted unreferenced sig.fs; live Gforth loader uses render, sigparse, db and prims. Existing examples plus signature lookup and parse/render checks pass with Gforth0.7.9_20260610.
---

bootstrap/src/sig.fs is unreferenced beside live render/sigparse/db/prims modules. Delete sig.fs and stale unify.fs reference. Confirm bootstrap/habu-lib.fs load graph and existing bootstrap signature behavior; do not delete live bootstrap checker.
