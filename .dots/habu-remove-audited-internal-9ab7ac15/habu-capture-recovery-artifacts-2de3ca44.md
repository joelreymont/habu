---
title: Capture recovery artifacts once
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.816463+03:00"
closed-at: "2026-10-22T12:36:34.851401+03:00"
close-reason: "Triage 2026-10-22, DONE (STALE-CLAIM): BF-ARTIFACT-FIXPOINT is gone. build-fixpoint.f:1150-1166 promotes each stage output to the next producer, which is a real generation chain."
---

build-fixpoint BF-ARTIFACT-FIXPOINT repeats same host2-5 times until adjacent artifacts agree; output never becomes next producer. Capture once on production path; retain a dedicated two-run determinism check that fails first mismatch and actual generation-chain convergence. Existing recovery build/artifact tests; periodic no-binary check for recovery changes.
