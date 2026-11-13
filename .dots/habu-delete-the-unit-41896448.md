---
title: Delete the unit-family plans and the fixed reader
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T11:54:44.659661+02:00"
---

Problem: after the record contract's deletion (lane carl-xrec), nothing sets UNIT-FAM-FIRST, UNIT-FAM-CUR, UNIT-FAM-PROJECT-ON or UNIT-FAM-RAW (src/core/type-family.f), so the FILE family-plan projection and every branch on it are unreachable, and nothing calls CHECKER-REG:RECORD-FIXED-READ-XT (src/core/checker.f), so src/compiler/native/dict.f RETAIN-FIXED and its install are dead. Acceptance: the plans, their hooks, the fixed reader and RETAIN-FIXED are gone, each branch keeping the arm its zero value took; the engine builds and the tests that read seeded registries pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xunit
