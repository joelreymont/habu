---
title: Retire obsolete package compilation tools
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.763447+03:00"
closed-at: "2026-10-16T11:36:32.297237+03:00"
close-reason: Deleted the unused source-view and unit-compile tools, their tests and gate entries, and the source-view-only test branch. Preserved live native FILE tools and shared kernel hooks. Whole-source, native-unit FILE and object refusal tests pass; caller and gate-entry review passed.
---

Delete tools/native-source-view.f and tools/native-unit-compile.f, their exclusive tests, source-view branch of whole-source-read-child and gate rows. Current FILE path uses SOURCE-EXEC and loader observers. Carl owns native-* files and shared hooks; coordinate exact deletion diff before integration. Do not remove NUNIT-OBJECT or real native FILE contracts. Retire GUARD hook only after current caller trace with Carl.
