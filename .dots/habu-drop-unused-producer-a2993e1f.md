---
title: Drop unused producer rollback
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T08:52:20.426126+03:00"
---

Independent Astra build-driver review of48a1a8f8 found BUILD-PRODUCER:MARK/REWIND/COMMIT have no callers or package reopenings. They retain five dynamic buffers and speculative source-observation rollback after the definition driver switched to direct ordinary loading. Delete this abandoned path and its reset work; keep current source/compiler/checker digest behavior. Repository search confirms onlypackage declaration. Proportionate checkedload of build-producer plus integrated native-build consumers; no new tests for dead-code deletion.
