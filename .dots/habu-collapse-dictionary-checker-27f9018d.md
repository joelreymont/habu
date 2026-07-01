---
title: Collapse dictionary checker tail
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T10:11:33.736343+02:00"
---

Problem: after removing top warm launchers, macos-arm64-12x2 hot full test suite still reports native dictionary/checker gate phase 11909ms under contention; focused dictionary is ~8744ms, so this remains a full-DAG tail. Fix: profile GD-MAIN spans, batch remaining candidate/check CLI sentinels, keep only true duplicate/fail-closed CLI boundaries, and move pure dictionary/checker semantic assertions to in-process CHECK-ALL-ERRORS-BUF or direct resident APIs. Acceptance: dictionary/checker <=7000ms in Mac hot full suite, focused dictionary <=7000ms, no loss of duplicate-definition/structure/package fail-closed coverage, full suite green. Evidence to update: /tmp/habu-full-hot3.out current counters inner-hb=6 inner-hb-stdin=4 boundary=10 helper-spawn=38 slowest=native engine candidate validation 16169ms.
