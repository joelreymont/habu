---
title: Give captured children a defined stdin
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T09:00:21.540014+02:00"
---

Problem: lib/process-argv.f line 86 PROC-SPAWN-ARGV-CAPTURE passes -1 as the child stdin descriptor, so a captured child inherits descriptor 0 of the launching process. A bare-argv bin/hb child reads stdin to end-of-file before running its script (src/habu/stdin.f lines 1-4), so a gate launched with an open never-ending pipe as stdin blocks every bare-hb capture child until the deadline kills it. This exact mechanism produced the 2026-07-26 gate-red mystery: the red set equalled the bare-hb spawner set (runner-test, process-test, aot-call-report) while --load spawners were immune; HB_TMP-length and box-contention hypotheses were disproven experimentally. Invariant: the input universe of a captured child is defined by the test, never by the launching terminal. Exact behavior: when the caller does not supply stdin explicitly, the capture spawn opens /dev/null read-only and passes it as the child stdin, closing it after spawn; PROC-SPAWN-ARGV-STDIN-CAPTURE keeps its explicit-pipe behavior unchanged. Acceptance: a regression spawns a bare-hb capture child while descriptor 0 of the test process is an idle open pipe and requires return code 0 within the normal budget; removing the /dev/null substitution makes that regression hang or time out. Files: lib/process-argv.f, lib/process.f if the descriptor helper belongs there, focused process capture tests. Verify: bin/hb --load lib/test/runner-test.f and the process suite, plus typed-local and package diff gates on the change. Depends: none. Ownership: stdlib process capture spawn path only. Claim: unassigned.
