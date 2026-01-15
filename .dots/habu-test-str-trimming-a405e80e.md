---
title: Test string trimming
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:43:29.943477+02:00"
---

Create tests: (string-trim " " "  hi  ") => "hi", (string-left-trim " " "  hi  ") => "hi  ", (string-right-trim " " "  hi  ") => "  hi". Dependencies: habu-implement-str-right-b5d36d64. Verify: all 3 trim functions work.
