---
title: Expose directory entry types
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T20:33:35.004276+03:00"
---

Measured Otto profile /Users/joel/.cache/tmp/dots-profile/report.md For Habu: FS-LIST:EACH discards dirent d_type, forcing Dots lstat+stat on each entry,about47M instructions on dot show. Add typed entry-listing callback and public named DT constants while preserving EACH caller behavior and DT_UNKNOWN fallback obligation. Actual macOS/Linux layout facts verified at responsible OS boundary, not guessed across targets. Report proposes EACH-TYPED path/name/type callback; decide meaningful type. Real directory fixture with files/subdir/symlink and existing process-tree/build-cache consumers, no per-case servers; focused checks, independent review and installed public API for Otto to migrate.
