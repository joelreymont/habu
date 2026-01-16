---
title: "Enhance #C complex reader macro"
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:52.843689+02:00"
---

src/reader/parser.zig: Improve complex reader
- #C(real imag): read complex number
- Validate both parts are real numbers
- Handle float/rational/integer parts
- Add tests for various complex forms
- Est: 15 min
