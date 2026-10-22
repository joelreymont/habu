---
title: Resolve declaration publication clobbers
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T15:56:45.248160+03:00"
---

The body-reader lane reports three remaining clobber-lint findings outside its code: habu1 BNATIVEUNITPUBLISH holds x15 across EM-DEF-OCC:LAPPEND; habu2 EMIT-QUALIFY-DEF holds x17; EM-AOT-REGISTER-RECS holds x4. Reproduce on the unchanged combined source and trace actual callee clobbers before changing code. Fix genuine caller preservation or inaccurate effect metadata at its owner, without broad suppression. Existing clobber lint and real affected declaration/registration artifact E2Es must pass; add a behavior regression before a runtime fix only if current E2Es miss the actual failure.
