---
title: Fix coerce list to string
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T07:18:26.984509+02:00"
---

stdlib.habu - coerce function

(coerce '(#\a #\b #\c) 'string) hangs/crashes

Check the coerce implementation for list->string case.
May need list-to-string primitive or loop with char accumulation.

Test:
(coerce '(#\h #\i) 'string) => "hi"
