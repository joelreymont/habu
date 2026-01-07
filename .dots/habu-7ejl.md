---
title: "Bug: nested (or ... (and (or ...) ...)) compiles incorrectly"
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-06T07:21:38.952302+02:00"
closed-at: "2025-12-25 07:21:22"
close-reason: "Obsolete: Zig rewrite"
---

The condition `(or A (and (or B C) D))` in a cond clause evaluates incorrectly. When A=nil, B=nil, C=true, D=nil, the whole expression should be nil but was evaluating to true. Workaround: use explicit if instead of and. See habu0.lisp reader fix.
