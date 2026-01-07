---
title: Fix echo REPL input handling
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-04T12:34:15.204416+02:00"
closed-at: "2025-12-04T13:43:00.37014+02:00"
close-reason: ""
---

The echo REPL compiles and runs but doesn't echo input correctly. Output shows 'You typed:' with no content. Likely issue with read-line-stdin or buffer-to-string interaction.
