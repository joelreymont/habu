---
title: Delete dead bootstrap loader
status: closed
priority: 2
issue-type: task
created-at: "2026-08-02T19:27:07.856111+02:00"
close-reason: "Patch-equivalent result landed as fc1d2efbef82."
---

Why: bootstrap/src/habu.fs is an unreferenced forwarding loader for canonical bootstrap/habu.fs and preserves a duplicate entry path with no caller. Exact result: delete bootstrap/src/habu.fs with no replacement or migration. Dependencies: none. Owned result: the obsolete loader is absent. Package owner: none because no definition survives. Acceptance: repo-wide live search finds no bootstrap/src/habu.fs reference; canonical bootstrap/habu.fs recovery path remains green. Smallest owning check: HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh.

