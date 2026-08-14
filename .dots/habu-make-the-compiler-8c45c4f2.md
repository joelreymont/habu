---
title: Make the compiler leaves load standalone or say why not
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T11:20:33.538588+02:00"
---

Recorded in LESSONS (j landing item 4) but undotted, re-confirmed by the typed-locals audit: bare --load of test/compiler/native-elaborate.f and native-hir.f dies -8602 E-NDICT-KIND outside the gate image, while native-locals-scope.f runs standalone (verified). Either make the two leaves self-sufficient like their sibling, or document the gate-only contract in their headers - a test that cannot run the way its header implies undermines debugging. Files: test/compiler/native-{elaborate,hir}.f. Depends: none.
