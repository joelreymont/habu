---
title: make-instance function
status: closed
priority: 3
issue-type: task
created-at: "\"2025-12-29T16:06:47.447238+02:00\""
closed-at: "\"2025-12-30T14:44:34.252672+02:00\""
blocks:
  - habu-4717bf7f294fc3ad
---

Implement make-instance for CLOS object creation.
Location: stdlib.habu + runtime
Syntax: (make-instance class-name &rest initargs)
Examples:
  (make-instance 'point :x 10 :y 20)
Implementation:
  1. Allocate instance with slot storage
  2. Process initargs to fill slots
  3. Call initialize-instance
  4. Return instance
Blocked by: defclass
