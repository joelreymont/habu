---
title: Test setf with incf/decf/psetf
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T06:25:45.963467+02:00"
---

File: tests/ or /tmp - Verify that existing incf/decf/psetf/rotatef/shiftf macros work correctly once setf is implemented. These macros already exist in stdlib.habu and use setf, so they should work automatically. Test cases: (incf x), (decf (car y)), (psetf a 1 b 2), etc. Depends on: all setf place support.
