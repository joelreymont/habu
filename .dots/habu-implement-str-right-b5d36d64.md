---
title: Implement string-right-trim in stdlib
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:41.443755+02:00"
---

stdlib.habu: Add (defun string-right-trim (char-bag string) ...). Trim from right side only. Dependencies: habu-implement-str-trim-1aaca2a1. Verify: (string-right-trim " " "  hello  ") => "  hello"
