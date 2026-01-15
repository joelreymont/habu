---
title: Test string comparisons
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:43:23.668995+02:00"
---

Create tests: (string< "a" "b"), (string> "b" "a"), (string<= "a" "a"), (string>= "b" "a"). All should return t. Dependencies: habu-wire-str-comparison-54930816. Verify: all 4 ops work.
