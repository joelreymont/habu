---
title: Give gate pool children an explicit stdin
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T14:54:29.413153+02:00"
---

Problem: test/gate-pool.f:583 GT-POOL-SPAWN passes '-1 >FD' for the child's stdin. src/habu/habu1.f SPAWN-DUP2-ACTION skips the dup2 for a negative fd, so every pooled job inherits the gate process's own fd 0, and posix_spawn makes the job a process-group leader. Run the gate from a terminal and each job holds a tty it did not ask for; a job that spawns a BARE engine (no --load) then has that engine enter the REPL and stop on SIGTTOU as a background group. Measured on 2026-08-04 in test/pre-trust-defer.f, whose own child spawn had the same shape: from a pipe the case reported an exit code in 1.3s, from a pty it died on the 20s timeout (E-PROC-TIMEOUT, exit 67). That fixture now passes an explicit empty stdin, so nothing is red today - jobs launched with --load take the file-list path (src/habu/habu2.f, MODE-LOAD) and never reach the tty REPL branch - but the gate's verdict still depends on how the gate was launched. Acceptance: pool children get an explicit stdin (an empty pipe, as SUITE-RUN-STDIN already does for the sequential path) instead of the launcher's; a regression that runs a pooled job which reports its own fd 0 and requires 'tty no' under a pty launcher; full test/gate-stdlib.f --pool-slots 3 green from a pty AND from a pipe. Files: test/gate-pool.f (GT-POOL-SPAWN, GT-POOL-START-SLOT), lib/process-env.f (PROC-SPAWN-ARGV-ENV-* stdin plumbing), a new pooled fixture. Verify: bin/hb --load tools/launch-context.f -- child under 'script' shows 'ctx fd0 ... tty yes' today; the same check inside a pool job must show 'tty no' after the fix. Depends: none. Ownership: test/gate-pool.f. Claim: unassigned.
