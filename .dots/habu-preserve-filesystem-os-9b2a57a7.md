---
title: Preserve filesystem OS errors
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T21:52:48.018488+03:00"
---

Otto demonstrated Dots release rc67 silently on unreadable issue file: fs READ-ALL and FS-OPEN-DIR replace failed open-rd with E-FS-OPEN, FS-LIST replaces failed open/read with E-FS-LIST-OPEN/READ. Primitive SYS-PUSH itself collapses errno to -1 on current targets, so library-only errno capture cannot recover the cause. Preserve the original failing syscall cause at its responsible boundary through filesystem cleanup and expose a precise public API for Dots error mapping; no reopen/libc race or global process-shared last error. Required failures EACCES, ENOENT, ENOTDIR, ELOOP, ENAMETOOLONG; retain existing generic throws/caller compatibility unless an explicitly documented API requires change. Inspect OS return conventions and fs callers, choose small task/call-local cause representation, existing real filesystem E2Es before code, independent Astra review and matching-engine/native/convergence gates if ABI or codegen changes. Coordinate FS-LIST typed entries and current build batch; app migration belongs to Otto.
