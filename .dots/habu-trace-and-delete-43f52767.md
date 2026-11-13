---
title: "A saved image may keep a live HEAP record's pointer"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:12:26.557829+02:00"
---

Problem: src/core/heap.f:6-7 says nothing registers a HEAP record and asks a caller to release one before an image save; no test shows what an image saved while a record is live holds, or whether anything refuses that save (found by search in the ENGINE-ORIGIN cleanup, not traced). Acceptance: a test saves an image while a HEAP record is live and shows the save refused, or the cause is fixed so a saved image cannot hold the pointer. Depends: none. Claim: unassigned.
