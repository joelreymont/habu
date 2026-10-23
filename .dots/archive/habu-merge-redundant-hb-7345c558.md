---
title: Merge redundant hb-build test builds
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T15:39:53.524947+03:00"
closed-at: "2026-10-23T16:04:59.892446+03:00"
close-reason: "Landed: 3 rows retired, duplicate builds merged; registry 500/500 in 601 s at load 45-66"
---

Problem: audit group 1 found gate rows that build and run programs whose coverage another row already holds, so the registry spends minutes on duplicate builds and drivers.
Acceptance: the duplicate builds, runs and rows below are merged or removed with each distinct assertion kept: aot-inputs large manifest; hb-build-aot positive programs as one; repl-fresh asset and child in one app; stripped-chain open path in the chain; layout-fetch and trusted-make in one bundle build; replay prepared-code case and native-file-stack row; stripped-quotation-field folded into hb-build-stripped's pointer-mark image; large-source folded into hb-build-fixtures' builds; check-verify requires-back (subset of identity-subject). Batching body-uncalled and reserved-names is not done: measured at about 1 s combined, and batching would change the path under test.
Verification: each changed row passes on its own, and the full registry (bin/hb --load test/run.f) passes in less wall time than the 565 s baseline.
