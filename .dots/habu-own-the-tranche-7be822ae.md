---
title: "Own the tranche's selector capacity class"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T07:53:40.248592+02:00"
---

The recorder landing measured the never-before-seen 151: the biggest new class is E-A64SEL-CAP (-8366) x20 (lib/ptx/cg.f EMIT-XPOSE-OFF 608B, lib/process-env.f PROC-ENV-ROW-Z 645B lead). Long bodies exhausting a selector capacity - diagnose the exact cap (values? blocks? staging rows?), derive or lift per the recorder discipline. Files: src/compiler/native/select.f. Depends: none.
