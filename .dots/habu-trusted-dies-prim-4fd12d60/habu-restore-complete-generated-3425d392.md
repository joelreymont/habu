---
title: Restore complete generated-source certification
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-14T12:14:20.562581+03:00\""
closed-at: "2026-09-16T14:34:47.979004+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: The named SOURCE-BUF-PRELOADED path is gone, but the residue stands: generated-source certification must isolate candidate declarations and reject malformed bodies."
---

Owner cedar. Review of b9745c57: SOURCE-BUF-PRELOADED accepts an underflowing colon body between lib/string.f provided markers (ordinary rc 70; preloaded rc 0), including absent or mismatched closing markers. Replace filename-based skipping with candidate declaration isolation; verify every generated body and reject malformed source. Preserve ordinary duplicate-declaration rejection. Cover stage2, stdin and snapshot certification through their real call paths.
