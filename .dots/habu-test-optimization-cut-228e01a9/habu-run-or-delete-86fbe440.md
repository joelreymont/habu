---
title: Run or delete test/native-unit-e2e.f
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T13:04:06.501417+02:00"
---

Problem (lane 297 nbimage): test/native-unit-e2e.f ('Explicit NBR package artifact acceptance') is referenced by no gate row, suite list or doc (rg -n native-unit-e2e finds only the file itself), so nothing runs it. Acceptance: run it alone on the current engine; if it checks a behavior no gate row covers, register it as a row (or merge its cases into the row that owns that behavior) and it passes; otherwise delete it and say which row covers each of its checks. Files: test/native-unit-e2e.f, test/gate-stdlib-cases.f.
