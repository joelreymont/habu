---
title: Fix Intel check-verify failures and cost
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T13:51:45.465595+00:00"
---

Owner: pulsar /root/check_verify in .jj-ws.noindex/pulsar-intel-check-verify over master/Intel origin reconciliation. Existing row has uses-visit count 11 versus 10, trusted-tick diagnostic ordering differs at tier 1, and baseline 1082 s user exceeds unchanged 360 s CPU budget. Reproduce through real verifier path, fix responsible production code, preserve master assertions and budgets. Acceptance: unchanged row green within budget plus focused downstream checks and independent review. Shared changes require an OPEN Mac ARM check.
