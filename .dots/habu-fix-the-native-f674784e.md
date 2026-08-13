---
title: Fix the native-elaborate standalone load
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T20:23:22.322267+02:00"
---

Found by the locals-scope lane, reproduced on unmodified master 0c8cac68: bin/hb --load test/compiler/native-elaborate.f is red standalone - LITKIND-CASE throws -8602 E-NDICT-KIND (dict.f:228) because CELL-A has no definer kind in a standalone load. The gate entry is green; only the file's own Run: line is broken - a test that cannot be run the way its header says undermines debugging. Files: test/compiler/native-elaborate.f. Depends: none.
