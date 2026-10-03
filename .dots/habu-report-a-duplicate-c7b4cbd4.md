---
title: Report a duplicate from VERIFY-BYTES and --verify-only
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:18:22.284427+03:00"
---

CHECK:VERIFY-BYTES (the language server's path) answers 'refused' for a duplicate with no packet, only the stderr line 'verification stopped by throw 78 after 0 rejected definitions'; check.f --verify-only reports the same throw 78 as rc 70, unlocated. Both should give the located duplicate record that --all-errors writes (docs/repair-diagnostics.md). Found by the Fable reviews pdup-rev5/rev8 while landing 1ca23983.
