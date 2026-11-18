---
title: Delete the marked rollback cells
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:04:09.602147+01:00"
blocks:
  - habu-a-caught-failed-9bb3e339
---

Problem: once habu-a-caught-failed-9bb3e339 gives a caught failed text's storage no longer back, src/core/rollback-cells.f's marks put back cells a retired word a stored token reaches still uses: putting back TASK-USER-NEXT or FN-N hands a task slot or an FFI index out twice. The 16 MARK calls (src/core/generated-declaration.f, src/lib/error.f, src/lib/task.f, src/lib/fork-child.f, src/lib/ffi-callback.f, src/lib/ffi-abi.f) and src/habu/interpreter.f FRAME-CELLS, FRAME-CELLS-RESTORE and FRAME-CUT exist only for that. Acceptance: ROLLBACK-CELLS and its requires, the marks, FRAME-CELLS, FRAME-CELLS-RESTORE, FRAME-CUT and the frame's trailing area are gone; DATA-SPACE:REWIND sits beside its one remaining caller, the declaration rollback; ERROR:CODE accepts a name and code a caught failed text registered again. Depends: habu-a-caught-failed-9bb3e339. Claim: unassigned.
