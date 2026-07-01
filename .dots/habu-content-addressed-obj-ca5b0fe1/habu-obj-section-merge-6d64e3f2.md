---
title: Object section merge buffers
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:39:51.922888+02:00\\\"\""
closed-at: "2026-07-01T21:43:09.777093+02:00"
close-reason: "completed: OBJLINK now decodes validated text/data hex rows into bounded merged section buffers, exposes TEXT$/DATA$, validates append totals, and fails capacity overflow. Proof: object-link-test, stdlib-manifest-test, typed-local-diff-lint, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18500ms <= 40000ms."
---

Problem: OBJLINK computes text/data sizes but does not retain merged section bytes, so a future object linker still cannot emit an image without rescanning and decoding object records. Fix: extend lib/object-link.f with bounded merged TEXT/DATA byte buffers, decode validated OBJ text/data hex rows during ADD, append in object order, and expose TEXT$/DATA$ slices. Keep size/layout counters as the authoritative byte counts and fail E-OBJ-CAPACITY on merged buffer overflow. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: merged text/data bytes preserve object order; empty sections work; text/data accessors return counted buffers; section decode rejects impossible malformed hex through existing OBJ validation; capacity overflow fails; existing symbol/effect/reloc checks still pass. Verify object-link test, typed-local-diff-lint, manifest/lints, full native suite before master.
