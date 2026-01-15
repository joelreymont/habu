---
title: Test stream operations
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:43:35.480804+02:00"
---

Create test: (with-input-from-string (s "hello") (read-char s)) => #\h, (with-output-to-string (s) (write-string "test" s)) => "test". Dependencies: habu-wire-stream-primitives-b9f6f9c0. Verify: streams work.
