---
title: Implement with-open-file macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:04.981109+02:00"
---

stdlib.habu: Add (defmacro with-open-file ((var filespec) &body body) ...). Expand to unwind-protect with open/close. Dependencies: habu-implement-close-for-4c6d390b. Verify: (with-open-file (s "f") (read-line s)).
