---
title: Implement string-trim in stdlib
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:32.626409+02:00"
---

stdlib.habu: Add (defun string-trim (char-bag string) ...). Trim characters in char-bag from both ends. Use char-position helpers. Dependencies: none. Verify: (string-trim " " "  hello  ") => "hello"
