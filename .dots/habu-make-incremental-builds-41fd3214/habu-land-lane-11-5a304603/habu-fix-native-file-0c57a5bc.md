---
title: Fix native-file-replay under lane 11
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T16:10:03.262604+02:00"
---

Problem: test/native-file-replay-e2e.f (lane 11 edits it), TFAIL asserts 108 221 225 233 246 259. Lane 11 (lqwqmsnq bf1b129f) fails it singly on its native-build.f candidate; master 7a3acd4c candidate (carl-base) passes it. Acceptance: root cause at the responsible layer, fixed as a fold-in into lqwqmsnq; the file rc 0 on a rebuilt candidate; the row it belongs to rc 0. Depends: none. Ownership: lane 11 code the cause names. Verify: bin/hb --load test/native-file-replay-e2e.f. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-rrp.
