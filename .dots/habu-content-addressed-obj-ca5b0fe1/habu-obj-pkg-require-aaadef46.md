---
title: Object package require tables
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:54:17.402028+02:00\\\"\""
closed-at: "2026-07-01T21:57:44.643988+02:00"
close-reason: "completed: OBJLINK now preserves package and require metadata with counts/accessors and capacity failures, without enforcing require closure yet. Proof: object-link-test, stdlib-manifest-test, typed-local-diff-lint, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18579ms <= 40000ms."
---

Problem: OBJ records carry package/require rows but OBJLINK discards them, so the future linker/cache cannot preserve dependency and visibility metadata across object loads. Fix: extend lib/object-link.f with bounded package and require tables, copy package name+visibility and require strings during ADD, expose counts/accessors, and cover capacity failures. Do not enforce require closure yet because require rows may name source paths or packages; this slice preserves checked metadata. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: package and require rows survive later OBJ:LOAD calls; accessors bounds-check; package/require overflow throws E-OBJ-CAPACITY; existing link checks still pass. Verify object-link test, typed-local-diff-lint, manifest/lints, full native suite before master.
