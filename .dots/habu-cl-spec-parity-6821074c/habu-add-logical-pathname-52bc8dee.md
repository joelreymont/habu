---
title: Add logical pathname translations
status: open
priority: 2
issue-type: task
created-at: "2026-02-05T12:16:22.994961+01:00"
---

docs/cl-symbols.md:1093 marks load-logical-pathname-translations missing. Root cause: logical-pathname-translations exist but loader not wired to filesystem. Fix: implement load-logical-pathname-translations to read translations file for host, update logical-pathname-translations table; add tests using temp files.
