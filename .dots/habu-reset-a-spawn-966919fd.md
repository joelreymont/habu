---
title: "Reset a spawn's staging when the spawn is refused"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:53:44.080210+02:00"
---

Problem: staged argv/env/cwd rows survive a refused non-capture spawn: staging a row, then PROC-CWD:SPAWN-ARGV-ENV-CWD-IO with a cwd of one NUL byte throws E-PROC-PATH, and the next RUN-ARGV-ENV-CAPTURE of /bin/echo prints the stale row. The non-capture spawns reset only after the spawn returns (src/lib/process-argv.f:101-105, process-env.f:281-286, process-cwd.f's IO words), and the RUN-* capture words' refusals before the spawn (process-cwd.f:77-82) leave staging too. Acceptance: every spawn and RUN word resets its staging whether it returns or throws; a test shows a refused spawn leaves no row for the next. Depends: none. Claim: unassigned.
