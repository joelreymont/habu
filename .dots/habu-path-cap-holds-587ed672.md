---
title: "PATH-CAP holds Linux paths to Darwin's limit"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T17:58:10.063181+01:00"
---

Problem: PATH-CAP is 1023 on every target (c453e484), Darwin's PATH_MAX less its NUL, because src/habu/layout.f's per-task FS-ABI and FS-MUT-ABI bands hold PATH-CAP buffers inside USER-REGION-END ($8000). Linux accepts paths up to 4095 bytes, so Habu refuses 1024-4095-byte paths its OS accepts. Acceptance: on each target Habu accepts every path length its OS accepts, without a per-task band sized for the largest. Depends: none. Claim: unassigned.
