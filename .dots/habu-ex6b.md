---
title: Reader parses 1+ and 1- incorrectly
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-06T21:16:20.131291+02:00"
closed-at: "2025-12-06T21:24:44.571344+02:00"
close-reason: ""
---

Habu reader parses (defun 1+ (n) ...) as (defun 1 + (n) ...) - treats 1+ as number 1 followed by symbol +. Should read 1+ as a single symbol.
