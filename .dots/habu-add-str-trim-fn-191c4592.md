---
title: Add string-trim function
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:25:15.831584+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (string-trim chars string) -> string with chars removed from ends
Need primitive string operations or char iteration.
Test: (string-trim ' ' '  hello  ') => 'hello'
