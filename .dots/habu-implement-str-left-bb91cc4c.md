---
title: Implement string-left-trim in stdlib
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:36.924258+02:00"
---

stdlib.habu: Add (defun string-left-trim (char-bag string) ...). Trim from left side only. Dependencies: habu-implement-str-trim-1aaca2a1. Verify: (string-left-trim " " "  hello  ") => "hello  "
