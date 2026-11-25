---
title: Keep load-time scratch out of program images
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T15:52:54.089537+01:00"
---

Problem: file-level byte buffers that a load writes keep their bytes in a program image (src/core/compile-reads.f:81 STAT-BUF, src/os/env-base.f:80-84, src/lib/process-cwd.f:16, src/lib/process-tree.f), so two builds of one source differ by a file's access time, the builder's TMPDIR or a pid. Acceptance: the declarations use the scratch storage src/core/pointer-storage.f gives, the two builds are the same, and a test in test/file-objects.f fails without it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-band2 (two lanes, one feature).
