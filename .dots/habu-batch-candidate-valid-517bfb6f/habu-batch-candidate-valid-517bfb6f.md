---
title: Batch candidate validation engines
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T15:21:41.979309+02:00\""
---

Parent habu-restore-30-second-fcadd9b9. RCA evidence: full gate stats /var/folders/98/l2ptpkyn41q7d3sp6x4xp87m0000gn/T/hb-gate-1979512594998041-7/gate-stats.tsv report 182 helper-spawn rows, native engine candidate validation 44720ms, runtime 23298ms, post-candidate 47110ms, and tail-process 31726ms. test/gate-engine-lib.f:1864-1884 serially drives 17 validation groups; their ON helpers at lines 356-680 make 53 direct candidate/bin-hb process launches before nested suite children. docs/gate.md step 8 still says batch candidate-source probes and helper-spawn <=25, but no executable structural limit exists. Implement digest-exact batching/transactional isolation at the candidate-source owner, preserve candidate-versus-baseline differential evidence and every negative exit contract, reduce full helper-spawn to <=25, and add a hard structural regression that cannot drift by profile. Acceptance: focused fixture/runtime/validate slices green; full exact gate helper-spawn <=25 and <=30s in three isolated calibrated macOS runs; no removed coverage or fail-open cache.
