---
title: Add string< and string> functions
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:25:11.381611+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (string< s1 s2) -> t if s1 lexicographically < s2
Need primitive string-compare or char-by-char comparison.
Test: (string< 'abc' 'abd') => t
