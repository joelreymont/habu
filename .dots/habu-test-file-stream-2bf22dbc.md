---
title: Test file stream operations
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:43:41.935182+02:00"
---

Create test: (with-open-file (s "/tmp/test.txt" :direction :output) (write-line "test" s)), then read back. Dependencies: habu-test-stream-ops-4b422101. Verify: file I/O works.
