---
title: Implement with-output-to-string macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:16.291441+02:00"
---

stdlib.habu: Add (defmacro with-output-to-string ((var) &body body) ...). Capture writes to string. Dependencies: habu-implement-with-input-ab995ffd. Verify: (with-output-to-string (s) (write-string "hi" s)).
