---
title: Remove def-create, which no definer calls
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.770860+02:00"
---

After kinds-a lands, definers.f no longer calls the def-create primitive. Its remaining traces: prims.f :929-931 rows, habu2.f DEFWRITE:DEF-CREATE and its FPRIM-WID row (:13916), kernel-x64.f :5818 refuse row, tools/native-unit-object.f :2649. Needs an x86-64 build proof.
