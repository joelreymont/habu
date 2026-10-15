---
title: Delete dead library words and wrapper layers
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.034956+03:00"
---

Problem: dead words: src/lib/build.f:56, 78; argv.f:330; process-cwd.f:53; process.f:987; memory.f:58; object-link.f:781; ffi-abi.f:128, 489, 537, 554; zip-state.f:31; process-pty-handle.f:362; test/record.f:46; src/os/image-bytes.f:146, 152, 212. The C2-MEM package is split across three files (c2-memory.f, with c2-owner.f reopening it). process-command.f:436-502 PROC-CMD re-exports 15 CMD words over CTX0 for three tools. Acceptance: each dead word deleted after rg across src, tools, test and ../etch; C2-MEM in one file; the three tools call CMD words directly and PROC-CMD is deleted. Files: those named, plus the three PROC-CMD tools. Verify: suites for the touched libraries. Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. (ffi-abi.f and image-bytes.f are in it; recheck callers against it.) Ownership: listed files. Claim: unassigned.
