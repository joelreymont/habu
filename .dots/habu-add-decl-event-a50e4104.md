---
title: Add decl-event.f to stdin-closure SSOT and cache key
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T06:59:26.054283+02:00"
---

Found by the runentry RCA lane (2026-07-21): src/core/decl-event.f is baked into the stdin closure via BF-APPEND-RUN-PRELUDE in tools/build-fixpoint.f (hardcoded path) but is MISSING from the closure single-source-of-truth manifest tools/stdin-closure-lib.f and its consumers test/run-files.f (TR-UNDER-SOURCE-FILES - the candidate content-cache key) and tools/srclist.f. The drift lint tools/stdin-closure-lint.f (gate 17e) reports 0 drift only because the file is absent from its own SSOT, so the omission is invisible. Consequence: a decl-event.f-only edit does NOT invalidate the cached gate candidate - stale candidate reuse with silently wrong engine contents. Fix structurally: add src/core/decl-event.f to the SDC manifest with its correct role, make build-fixpoint.f source it through the SDC accessor instead of the hardcoded string, and extend stdin-closure-lint to cross-check build-fixpoint.f's actual baked file set against the manifest so a future hardcoded bypass is impossible. Regression: prove a decl-event.f edit changes the candidate cache key. Verify: stdin-closure-lint, run.f candidate build, fixpoint x2.
