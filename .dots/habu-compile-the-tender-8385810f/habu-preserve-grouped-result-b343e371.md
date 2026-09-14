---
title: Preserve grouped result rows through native finally
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T13:27:14.065195+03:00"
---

Maki replacement acceptance 2026-09-14: native compile of KIAPI:FOOTPRINT-POSE ( item -- point angle side ) throws -8503 E-NELAB-JOIN. Minimal application reproducer requires src/kiapi/items.f after maki.f, then tools/hb-build.f --repl. Body CLAIM, stores item, ticks POSE-BODY and RELEASE, finally. Full Maki suite and plain/error-package image restore pass on frozen9d0a44a3 pair; candidate not accepted. Reduce grouped output results, fix native finally call glue, cover execution and failure cleanup without changing application types.
