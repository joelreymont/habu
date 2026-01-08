---
title: Add setf support for nth and elt
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-08T06:25:31.819310+02:00\""
---

File: lib/stdlib.habu - Extend setf to handle list/sequence access: (setf (nth n list) val) needs to walk list and rplaca at position, (setf (elt seq n) val) dispatches on sequence type. May need helper %set-nth primitive or implement in macro. Depends on: basic setf implementation.
