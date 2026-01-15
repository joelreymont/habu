---
title: Implement with-input-from-string macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:10.815225+02:00"
---

stdlib.habu: Add (defmacro with-input-from-string ((var string) &body body) ...). Bind var to string-input-stream. Dependencies: habu-implement-with-open-eff1a2ca. Verify: (with-input-from-string (s "hi") (read-char s)).
