---
title: Fix handler-case macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T07:18:15.573533+02:00"
---

stdlib.habu - handler-case

(handler-case (error "test") (error (c) 'caught)) fails

Check handler-case macro expansion and ensure it properly
catches signals from error function.

Test:
(handler-case (error "oops") (error (c) 'caught)) => caught
