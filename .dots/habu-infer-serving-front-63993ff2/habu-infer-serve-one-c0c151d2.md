---
title: "Infer serve: one-command launch"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.376529+02:00"
blocks:
  - habu-infer-serve-line-80a4c28d
  - habu-infer-serve-concurrent-a5e7de35
  - habu-infer-dense-safe-3b25bdfa
---

Why this exists:
Users need one command that validates and loads a pack, chooses the requested transport, starts serving, and shuts down cleanly.

Required result:
Compose pack loading, capacity planning, engine construction, scheduler creation, line or HTTP transport startup, signal handling, and final cleanup behind one documented command.

Done when:
A valid pack serves a fixed request after a fresh start; invalid configuration fails before publication; startup interruption and shutdown release every mapping, descriptor, request, and cache owner; restart is reproducible.

Expected touch points: the serve command, focused process tests, and user documentation.
Smallest check: one fresh-process request over each supported transport.
Prerequisites: line protocol, concurrent HTTP server, and safe dense-model capacity boundary.
Owned result: serving command composition and shutdown only.
Claim: unassigned.
