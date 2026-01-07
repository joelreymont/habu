---
title: Investigate habu0 crash at MAIN+620 (CAR on nil)
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-07T21:04:04.966534+02:00"
closed-at: "2025-12-09T13:10:09.93616+02:00"
close-reason: ""
---

Debug crash at address 0x29304 (MAIN+620) where CAR operates on nil; investigate null-check/neg encoding and x20 env handling; context files: habu0.lisp main around line 2461, bootstrap/reg-alloc.lisp x20 restore around line 1504, habu0.map symbol map.
