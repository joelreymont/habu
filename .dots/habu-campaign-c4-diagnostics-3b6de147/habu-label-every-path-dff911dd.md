---
title: Label every path range a loader rejects
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.188387+01:00"
closed-at: "2026-11-16T04:07:35.619613+01:00"
---

Problem: PATH-RANGE's callers ROOT-SET (include.f ~:653) and verify-source.f (~:4421) may write a record with no label. Acceptance: each caller's record has a label, shown by a probe each. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
