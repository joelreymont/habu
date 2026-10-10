---
title: Make cast-negative spans engine-size independent
status: open
priority: 3
issue-type: task
created-at: "2026-10-10T10:11:04.263748+03:00"
---

Problem: test/cast-negative-suite.f case 23 failed (expected 28, got 24) on a probe engine of the return-stack lane and passed on its final engine: its spans include `constant` definitions whose code size depends on the address they hold, so the expectation changes whenever the engine's size moves (measured in ~/.cache/tmp/carl-rsin/handoff.md).
Acceptance: the case asserts what the cast rule decides, not a code size that depends on where the engine places data; it passes on engines of different sizes.
Files: test/cast-negative-suite.f.
Verify: `bin/hb --load test/cast-negative-suite.f` on two engines of different image size; `bin/hb --load test/run.f`.
Depends: none. Worker: worker-light.
