---
title: Reuse checker signature arenas after snapshot
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-30T21:23:24.835066+02:00\""
closed-at: "2026-06-30T21:31:43.623852+02:00"
close-reason: done
---

Problem: heap-backed USIGS fixes snapshot bloat, but USIGS-RESET currently calls USIGS-RUNTIME-INIT and can mmap a fresh 8MiB arena even when the existing runtime arena is already large enough. Fix: make checker signature reset reuse the current arena when USIGS-P is nonzero and USIGS-CAP-U >= USIGS-INIT-CAP; allocate only when the snapshot compact payload is too small or no arena exists. Add/adjust a focused checker/engine test that proves reset preserves capacity/pointer when already runtime-sized and still allocates from a compact/small capacity. Files: src/core/checker.f, test/engine-suite.f or a focused checker test. Verify: focused engine/checker test, typed-local diff lint, native refresh still fixpoints, warm-image-test still passes.
