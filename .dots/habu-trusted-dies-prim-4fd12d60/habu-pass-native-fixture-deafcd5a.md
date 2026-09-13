---
title: Pass native fixture scratch paths without shell splitting
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T14:51:13.081324+03:00"
---

Cedar review of 6317023977b2 on 5226a994: test/nf.fs:64-74 builds shell commands from unquoted HB_TMP-derived paths. NF-RUN with an existing HB_TMP containing a space exits 1 at slurp-file; identical space-free control prints 42. NF-REPL-CMD also combines three paths into 256 bytes though each path may be 128; a 97-byte HB_TMP passes path checks then exceeds command capacity. Quote each complete shell argument and size for escaped inputs, or avoid shell parsing. Keep parallel fixtures isolated. Unassigned.
