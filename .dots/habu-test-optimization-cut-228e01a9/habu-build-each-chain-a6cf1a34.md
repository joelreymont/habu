---
title: Build each chain generation with the one before
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T17:17:49.812538+02:00"
---

Problem: tools/chain-run.f MAIN (:105-108) passes `0 ARG` (the host) as the engine for all three BUILD calls, while its header (:7-8) says a second generation is built from the first and a third only when they differ. Both generations come from the host and the same source, so SAME? compares two host builds and the chain never tests a fixpoint. Acceptance: gen 2 is built by gen 1 and gen 3 by gen 2; a case shows the chain reporting a non-fixpoint when gen 1 differs from what it builds (e.g. a host whose output differs from its own build) and a fixpoint otherwise; tools/chain-run-build.f's statuses unchanged. Files: tools/chain-run.f and its test. Verify: the case; tools/chain-run-build.f on a real host rc 0.
