---
title: Fix Intel check-verify failures and cost
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T13:51:45.465595+00:00"
closed-at: "2026-11-04T11:48:48.109285+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. check-verify on the tier-1 x86 intel engine (1082 s vs 360 s budget, tier-1 tick ordering); pulsar-intel-check-verify not listed."
---

Owner: pulsar /root/check_verify in .jj-ws.noindex/pulsar-intel-check-verify over master/Intel origin reconciliation. Existing row has uses-visit count 11 versus 10, trusted-tick diagnostic ordering differs at tier 1, and baseline 1082 s user exceeds unchanged 360 s CPU budget. Reproduce through real verifier path, fix responsible production code, preserve master assertions and budgets. Acceptance: unchanged row green within budget plus focused downstream checks and independent review. Shared changes require an OPEN Mac ARM check.
