---
title: Name what triggers each file cache refusal
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T12:51:54.661178+02:00"
---

Engine cold build (ulxuqysn) stores 33 of 194 files. 126 frames are ordered=1: 83 reason=mapped-compile-time-read word=ROW-LENS (the require registry's mapped column, src/core/include.f:36, reached through an executed word's closure in CAPTURE-RUN/REACH-DATA), about 28 run-host-unchecked through typed storage definers (PTR-VARIABLE, PERSISTED-PTR-VARIABLE, RESERVED-PTR-U8-CELL...). 39 files refuse tier and 32 earlier-write-before-load with no trigger named. Lane: diagnostics-only lines ordered-run (reason/holder/via/run), earlier-write (site owner), tier (value), then a cold census. The ranking feeds habu-refuse-unrecordable-compile-aa3c25a5. Workspace: carl-census, brief ~/.cache/tmp/carl-file/brief-trigger.md.
