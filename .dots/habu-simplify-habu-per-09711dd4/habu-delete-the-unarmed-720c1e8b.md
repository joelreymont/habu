---
title: Delete the unarmed tree shaker
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.757356+03:00"
---

Problem: The shaker was never armed in production; its KEEP? gates always kept every primitive. The reachability scan was test-only and had an unchecked buffer write. Delete the shaker and load entries, emit the kept branches directly, retain the checker span tests, and correct affected docs and pending design notes.

Claim: mark-tree-cleanup workspace from 2e2a1f98, authorized by Carl. Keep active until Carl's combined native qualification and integration. The owner of that qualification closes this dot.
