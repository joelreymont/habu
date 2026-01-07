---
title: Fix h0-eval op= string comparisons - use cached symbols like other operators
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-08T19:05:00.671922+02:00"
closed-at: "2025-12-08T20:04:40.94654+02:00"
close-reason: ""
---

h0-eval has ~21 operators using generic `(op= op "STRING=")` which calls intern->find-interned->string=. This creates circular dependency and breaks these operators. Should use cached symbols like `*op-string=*` with `(op=string= op)` using eq, same pattern as op=plus, op=minus, etc. Operators needing fix: SYMBOLP, NUMBERP, STRINGP, STRING-LENGTH, STRING-REF, STRING=, SYMBOL-NAME, LOGAND, LOGIOR, ASH, EQ, EQL, GET-TAG, LENGTH, MAKE-VECTOR, VECTOR-LENGTH, VECTOR-SET, VECTOR-REF, REVERSE, MAKE-STRING-FROM-VECTOR, MAKE-SYMBOL-FROM-STRING
