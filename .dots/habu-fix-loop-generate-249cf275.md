---
title: Fix loop-generate-code causing stdlib load failure
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T16:49:08.649606+02:00"
---

stdlib.habu:3755-3765: call to loop-generate-code (24 params) causes 'Error expanding macro do' with TypeMismatch. Binary search found exact failure at line 3765. Cause unclear - may be related to macro expansion during defun compilation, or interaction between case/cond/do macros. Workaround: refactor to use struct/plist for parameters. See habu-fix-stdlib-habu-0ac19837 for investigation history.
