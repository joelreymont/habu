---
title: Restore 30-second native gate
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-17T13:24:55.150379+02:00\""
---

Stop-line regression: verified master on DGX Spark completes every native correctness phase but takes 27.735 seconds with the JSON throughput ratchet removed, exceeding the calibrated 27.500-second cold wall. The critical path is the 7.591-second candidate build followed by the 20.218-second Maki core slice. The Maki slice contains measured redundant work: cad-replay-test reruns the complete cad-test suite and costs 4.365 seconds after cad-test already ran.

Own only measured removal of redundant production-gate work. Do not raise or bypass the elapsed or wall limits, prime caches with a failed attempt, reduce coverage, retry hard failures, hide work outside an end-to-end clock, or add another hand-maintained Maki partition. Child habu-run-focused-cad-ba7e3860 owns the first exact repair. Re-measure the complete cold gate after that child lands before admitting another optimization.

Acceptance: three isolated cold-cache exact-tree runs pass the existing Spark nominal and wall limits with stable candidate identity and complete correctness coverage. Warm and cache-hit paths retain their lower limits. Standalone Maki, PTX standard library, recovery and fixpoint, host, file-map, dot, and full native gates pass. Any remaining timing work must be a separately measured small child at its architectural owner. This controller is unclaimed; only implementation leaves become active.
