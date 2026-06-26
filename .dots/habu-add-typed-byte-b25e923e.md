---
title: Add typed byte pointer offset
status: open
priority: 2
issue-type: task
created-at: "2026-06-24T13:30:07.780788+02:00"
---

lib/fs.f now uses trusted FS-BYTE-OFFSET for ptr u8 plus byte offsets because the checker models raw + as numeric for input pointers. Add a checked primitive/model for byte pointer offset (ptr u8 n -- ptr u8), replace FS-BYTE-OFFSET call sites, and keep fs dirent/stat reader regressions passing under the native prelude.
