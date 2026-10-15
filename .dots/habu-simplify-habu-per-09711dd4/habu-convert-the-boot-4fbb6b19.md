---
title: Convert the boot-census tools to Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.162706+03:00"
---

Problem: tools/perf/boot-census-watcher.c (152 lines, kqueue EVFILT_PROC) and boot-census-analyze.py (77 lines) carry logic outside Habu, against the CLAUDE.md rule; docs/debugging.md:705 sends process-count questions to them. Acceptance: one checked Habu tool follows a command's process tree and classifies children fork-vs-exec by image path and argv, matching the current output on a sample run; the C and Python files deleted; docs/debugging.md updated. Files: tools/perf/*, docs/debugging.md. Verify: run on a small hb command and compare with the old tool's output. Depends: none. Ownership: listed files. Claim: unassigned.
