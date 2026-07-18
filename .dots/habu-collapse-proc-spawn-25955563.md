---
title: Collapse PROC-SPAWN combinatorial matrix into PROC-SPEC record
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:40.510872+02:00"
---

Depth review: 4 marshaling prefixes x ~5 completion modes ~= 20 public words (process.f, process-argv.f:35-94, process-env.f:106-304, process-cwd.f:8-41); names like PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE are the cross-product smell; each new axis grows multiplicatively (cwd-without-env already missing). One PROC-SPEC PRODUCT record (pathz/argv/envp/cwdz/fds) + ~7 words; cluster is under active wave-B/C ADT work so the collapse fits the switchover idiom. ~200 lines + surface-growth arrest.
