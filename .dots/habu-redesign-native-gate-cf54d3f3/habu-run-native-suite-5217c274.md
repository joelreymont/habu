---
title: Run native suite directly from bin/hb
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:07.423419+02:00\""
close-reason: "completed: test/run.f now runs directly from bin/hb, no hb-test-suite top snapshot exists, docs/skills no longer describe hb-test-suite, macOS hot proof passed at 27.392s internal / 29.71s shell wall with warm-miss=0"
---

Problem: test/run.f currently builds/reuses a large hb-test-suite warm snapshot to hide parent harness load. This violates the target architecture: reuse the small bin/hb engine and do not replace a 16s warm-runner rebuild with a 20s top-image rebuild. Fix: make test/run.f load the resident suite implementation directly in bin/hb, remove test/run-main.f/top-runner cache/key/build path, keep only small artifact caches for under-test/AOT/check/tool helpers that are not yet eliminated. Acceptance: bin/hb --load test/run.f runs without creating hb-test-suite, hot Mac wall remains under 30s or any regression is explained by measured remaining suite body work, docs/skills no longer describe hb-test-suite.
