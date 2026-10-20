---
title: Check image binding without indexing
status: active
priority: 2
issue-type: task
created-at: "2026-10-19T15:03:33.103842+03:00"
---

Artifact readiness and cached-object relinking use IMAGE-NAMES:LOAD-BOUND to verify the existing header and executable digest without indexing name rows; LOAD retains full indexing. The AOT cache E2E reuses the source-mutation fixture's A/B builds to reject a mismatched executable/map pair, relink B's cached object, then restore the same runnable executable and map byte for byte, removing two maker runs. The refolded cache E2E passes; engine-size-test and temporary binding/reset/header/IO probes passed before these review fixes. Focused Astra follow-up review and a normal engine artifact-hit measurement remain with the integrator; the measured tool-load reduction alone is not a whole-hit comparison.
