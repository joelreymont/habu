---
title: Capture recovery artifacts once
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.816463+03:00"
---

build-fixpoint BF-ARTIFACT-FIXPOINT repeats same host2-5 times until adjacent artifacts agree; output never becomes next producer. Capture once on production path; retain a dedicated two-run determinism check that fails first mismatch and actual generation-chain convergence. Existing recovery build/artifact tests; periodic no-binary check for recovery changes.
