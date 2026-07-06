---
title: Bake boot-prefix pin into image
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:00:00.000000+02:00"
---

Build-time half landed: BF-PIN in tools/build-fixpoint.f records each emitted
boot-prefix source file's content digest on first read (keyed by SHA-256 of the
path) and re-verifies on every reload across the stage2/stdin/snap emissions and
the stamp-key re-emit; a mid-build source edit throws E-BUILD-BOOT-DRIFT and
fails the build. Regression: tools/build-fixpoint-test.f `boot pin mismatch`.

Remaining (needs src/habu engine work, spec-and-stop): the habu-bf-certify-stage
phrasing "sha256 baked into image" for boot-TIME reload verification. The small
bin/hb engine reloads core/checker/tool source from the checkout at process
start (docs/bootstrap.md), so an image built/certified against source revision
A can boot against revision B. To close that: emit the combined boot-prefix pin
digest as a constant into the generated stage source, and have the engine
verify the on-disk boot prefix hashes to the baked value before use at startup.
That is an engine change (startup verification path), out of scope for the
build-tool-only work; this dot tracks it.

Ported from the fable lane (eb9ee4631166) onto the maki-type-families line.
