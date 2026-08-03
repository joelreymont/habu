---
title: Sweep timing assertions out of scheduled gates
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-03T16:22:48.129781+02:00\""
---

Cost-direction assertions (COSTLIER? and the T-SUM band at codegen-compare-test.f:747, flaking 1-in-10 idle, dotted habu-retire-the-flaky-25a37a74) are a flake class in scheduled gates. The proven replacement: deterministic emitted-code pins (the DS-STORES/DS-LOADS decoder pattern from the narrowing leaf - instruction counts move for exactly one reason). Sweep: every timing-direction assertion in a scheduled suite becomes a deterministic count pin or moves to the hand-run timed check's report; subsumes habu-retire-the-flaky-25a37a74 (close it with this).

Claim: agent=simplane workspace=.jj-ws/habu-collapse-the-harness-d1c4b1de
