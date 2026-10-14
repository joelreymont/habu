---
title: Fast-path ASCII UTF-8 validation
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T20:33:34.994894+03:00"
---

Measured Otto profile /Users/joel/.cache/tmp/dots-profile/report.md For Habu: UTF8:VALID? calls NEXT per byte,98.95 instructions/byte vs7.85 for ASCII path; experiment saves46M instructions on Dots ls --json. Add bounded ASCII fast path while preserving all valid/invalid UTF-8, source and continuation semantics, no overread or new parser. Existing utf8-scalar tests and real JSON/string consumers first; use mode utf8/text from jp2 report for unchanged-source measurement, independent review and proportional library checks.
