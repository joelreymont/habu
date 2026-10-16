---
title: Retire obsolete package compilation tools
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.763447+03:00"
---

Delete tools/native-source-view.f and tools/native-unit-compile.f, their exclusive tests, source-view branch of whole-source-read-child and gate rows. Current FILE path uses SOURCE-EXEC and loader observers. Carl owns native-* files and shared hooks; coordinate exact deletion diff before integration. Do not remove NUNIT-OBJECT or real native FILE contracts. Retire GUARD hook only after current caller trace with Carl.
