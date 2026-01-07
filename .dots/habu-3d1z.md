---
title: Verify TCO works in native Habu
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-04T23:01:08.659903+02:00"
closed-at: "2025-12-06T21:26:31.551424+02:00"
close-reason: ""
---

TCO (tail call optimization) is implemented in optimize.lisp and used in compile-forms via apply-tco-to-all-functions.

Need to verify:
1. Self-recursive tail calls are converted to loops in native mode
2. loop-ir and continue-ir codegen works correctly
3. Generated code actually avoids stack growth for tail-recursive functions

Test: Compile a deeply recursive function (e.g., count to 1000000) and verify no stack overflow.

Current blockers checked:
- apply-tco-to-all-functions is called in compiler.lisp (native)
- No #+sbcl guards on TCO code
- Should work but needs verification
