---
title: Restore complete generated-source certification
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T12:14:20.562581+03:00"
---

Owner cedar. Review of b9745c57: SOURCE-BUF-PRELOADED accepts an underflowing colon body between lib/string.f provided markers (ordinary rc 70; preloaded rc 0), including absent or mismatched closing markers. Replace filename-based skipping with candidate declaration isolation; verify every generated body and reject malformed source. Preserve ordinary duplicate-declaration rejection. Cover stage2, stdin and snapshot certification through their real call paths.
