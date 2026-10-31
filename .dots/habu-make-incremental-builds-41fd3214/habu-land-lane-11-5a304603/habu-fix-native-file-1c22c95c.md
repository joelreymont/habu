---
title: Fix native-file-admission under lane 11
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T16:10:03.274149+02:00"
---

Problem: test/native-file-admission-e2e.f, TFAIL assert 6. Lane 11 (lqwqmsnq bf1b129f) fails it singly on its native-build.f candidate; master 7a3acd4c candidate (carl-base) passes it. Acceptance: root cause at the responsible layer, fixed as a fold-in into lqwqmsnq; the file rc 0 on a rebuilt candidate; the row it belongs to rc 0. Depends: none. Ownership: lane 11 code the cause names. Verify: bin/hb --load test/native-file-admission-e2e.f, then the native-file-auto row. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-rad.
