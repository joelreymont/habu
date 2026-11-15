---
title: Drop the wide-name flag nothing reads
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T15:18:42.391197+02:00"
---

Problem: once top level carries multi-cell values (campaign step 3, commit 42df1432), nothing reads DNAME-WIDE (src/habu/layout.f:140), yet the kernels set it through wide-mark (habu1.f:3571-3578, kernel-x64.f:3602-3614), fold it into find's flags (habu1.f:5007, 5038; habu2.f:739, 1077, 2132; kernel-x64.f:4537, 5186) and the linker copies it (linker.f:5653). Acceptance: no DNAME-WIDE, wide-mark or bit-62 name flag left in the tree, and the engine builds itself. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-drop-the-wide-name.
