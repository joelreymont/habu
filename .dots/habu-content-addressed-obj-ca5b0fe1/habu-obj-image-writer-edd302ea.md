---
title: Object image writer
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T22:20:37.809472+02:00\\\"\""
closed-at: "2026-07-01T22:24:34.537214+02:00"
close-reason: "completed: added OBJIMG object-image writer that includes missing build seams, copies linked OBJ text into the assembler buffer, writes target images, and runs a generated exit(0) executable. Proof: object-image-test, typed-local-diff-lint, stdlib-manifest-test, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18497ms <= 40000ms."
---

Problem: OBJLINK can merge/apply object text but no checked build boundary can turn a linked object into a native target executable, so hb-build cannot eventually skip source compilation on an object-cache hit. Fix: add tools/object-image.f package OBJIMG that owns the build-internal boundary: reset/add OBJ records through OBJLINK, require non-empty linked text, copy OBJLINK:TEXT$ into the assembler buffer with BYTES,, then wrap it with ASM-CODE/BUILD-IMAGE/CODESIG2/DRV-WRITE-IMAGE. Add a focused test that builds a tiny exit(0) object, writes an executable image, and runs it. Files: tools/object-image.f, tools/object-image-test.f, FILEMAP.md, test/gate-stdlib-cases.f. Acceptance: generated object image runs with rc 0 on macOS and Linux target runners; empty linked text fails E-OBJ-SCHEMA; existing object/link tests still pass. Verify object-image-test, typed-local-diff-lint, manifest/filemap/lints, lint-artifacts-fast, full native suite before master.
