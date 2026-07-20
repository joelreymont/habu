---
title: Raise or right-size TR-CAP for the shared suite image
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-20T14:33:05.735544+02:00\""
---

Capacity wall found by the rank-0 registry lane (13de925c): the tensor registry cap TR-CAP = 64 (maki/extent-tensor.f) is nearly exhausted in the shared cold-gate image, where every maki suite loads into ONE image and TENSOR: rows accumulate across suites. The lane's first draft added 3 rows and overflowed the cap inside attn-eq-test - it shipped by reworking its tests to add ZERO new rows (reusing existing tensors), which is a workaround pattern that will not survive the next few suites; batched attention and the vision epics will mint many more TENSOR: declarations. Measure the current peak (the lane measured EQ-N at 80/128 for EQ-CAP; measure TR-N the same way at the end of a full test-core image load), then raise TR-CAP with headroom consistent with the memory budget (each row's size is visible at the declaration site), keeping the fail-closed named die at the new cap. Red-first: a cap+1 registration dies named before and after; a regression pins the behavior. Same shape as the REQUIRE-MAX raise (eb8ca85b + 94a0a25d): the fixed-cap+named-die idiom is correct, the value is outgrown. Territory: maki/extent-tensor.f + a capacity regression; measure DATA impact and update size rows if the table is image-resident.

Claim: agent=trcap workspace=.jj-ws/fable-trcap machine=spark (owns maki/extent-tensor.f TR-CAP + capacity regression)
