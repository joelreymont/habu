---
title: Fix case-insensitive symbol checks in compiler-sbcl.lisp
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-15T16:56:32.320516+02:00"
closed-at: "2025-12-15T16:58:13.554561+02:00"
close-reason: ""
---

3 locations check both uppercase and lowercase variants: (or (eq op 'LABELS) (eq op 'labels)). This indicates broken interning - should normalize at read time
