---
title: Preserve grouped result rows through native finally
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T13:27:14.065195+03:00"
---

Maki replacement acceptance 2026-09-14: native compile of KIAPI:FOOTPRINT-POSE ( item -- point angle side ) throws -8503 E-NELAB-JOIN. Minimal application reproducer requires src/kiapi/items.f after maki.f, then tools/hb-build.f --repl. Body CLAIM, stores item, ticks POSE-BODY and RELEASE, finally. Full Maki suite and plain/error-package image restore pass on frozen9d0a44a3 pair; candidate not accepted. Reduce grouped output results, fix native finally call glue, cover execution and failure cleanup without changing application types.

2026-09-14: DO-FINALLY now preserves checker-owned output grouping. Hazel review
caught missing/dead call-row handling; the first binary passed product tests but
failed to compile EXECUTABLE-BUILD:WITH (-8519). The final guard follows execute
when ordinary call rows are absent and skips grouping for a dead output. New
native-finally covers named/literal/callback products, saved values, both throw
paths, dead quotations, cleanup borrow refusal and the real build wrapper.
Fresh hb SHA c37b51ff3be1daf520cc0ba682403c87021618b5ff8e404eb26e4dd2143cbcda
passes this fixture at both tiers. Product-hosted rebuild and Maki acceptance
are still pending; the dot remains open until the application reproducer passes.
