---
title: Reuse capture table spans
status: active
priority: 2
issue-type: task
created-at: "2026-10-19T13:22:47.825677+03:00"
---

Representative warm self-build profile has repeated XTOFF-BUF getter/reserve and ADDRESS-CELLS ROW lookup overhead inside AOT capture loops; each ROW lookup re-runs ENGINE-ABI-XT dictionary resolution. Existing APIs already expose stable owned spans. Hoist those acquisitions within the proven non-mutating scan boundary, preserving callback ownership and all pointer/format refusals. Existing real capture artifacts and warm equality remain acceptance; measure changed loops rather than unrelated allocation targets.
