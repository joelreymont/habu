---
title: Refuse aot-call-report library misuse by throw
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T17:42:16.198912+03:00"
---

Found by review 470 (290ac608): tools/aot-call-report-lib.f is loaded in process by the AOT gates (CODE-REPORT) but ends the process on misuse: REPORT-BUFFER! dies 74 on a null or negative buffer (98-99; lib/errors.f E-JW-OUTPUT names exactly this), REPORT-FILE! (86), OPEN-INPUT (249), ACR-SCAN-ONE-READ (259) and JSON-NUM (162/169). Card section 6: die is for build makers and CLI boundaries; a library refuses by named throw its caller reports. Inert today (callers pass valid constants), so test through the library with a misuse case per site, seen failing first. Acceptance: each library-level die is a named throw (an existing code where one fits), the CLI (REPORT-MAIN) keeps its documented exits, and tools/aot-call-report-test.f covers each.
