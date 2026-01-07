---
title: String trim functions
status: closed
priority: 2
issue-type: task
created-at: "2025-12-29T16:06:22.868709+02:00"
closed-at: "2025-12-29T19:04:22.421841+02:00"
---

Implement string trimming functions.
Location: stdlib.habu or compile.zig
Functions:
  (string-trim chars string)
  (string-left-trim chars string)  
  (string-right-trim chars string)
Examples:
  (string-trim " " "  hello  ") => "hello"
  (string-left-trim "abc" "abracadabra") => "racadabra"
