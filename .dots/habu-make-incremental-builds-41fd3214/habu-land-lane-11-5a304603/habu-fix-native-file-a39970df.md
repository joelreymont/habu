---
title: Fix native-file-state under lane 11
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T16:10:03.239938+02:00"
closed-at: "2026-10-31T20:23:16.183777+02:00"
close-reason: "landed: lane 11 as 4b54ed21 Reuse compiled files in incremental builds, 9a25b36f, a8bdf2af on master; gate at the lane tip: native-build rc 0, registry rc 0 1164 s, P0 cold 164 compiled, warm 162 reused/2 compiled, warm == cold, two-generation rc 0; after rebase over ef2bf183 the overlapping rows (check, check-verify, gate-diagnostics, check-all-errors, repair-packet, load-reject-diag, runtime-regression, native-file-frame) rc 0"
---

Problem: test/native-file-state-e2e.f, TFAIL asserts 30-33 36-38 50 59-61 72 73 75 101 104, uncaught -8570 and -2500. Lane 11 (lqwqmsnq bf1b129f) fails it singly on its native-build.f candidate; master 7a3acd4c candidate (carl-base) passes it. Acceptance: root cause at the responsible layer, fixed as a fold-in into lqwqmsnq; the file rc 0 on a rebuilt candidate; the row it belongs to rc 0. Depends: none. Ownership: lane 11 code the cause names. Verify: bin/hb --load test/native-file-state-e2e.f test/native-file-retired-e2e.f. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-rst.
