---
title: "Linux arm64: memory-test throws -2502 on spark"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T08:58:57.271172+02:00"
closed-at: "2026-11-11T09:30:21.203123+02:00"
close-reason: "landed: the test sets its core limit to 1; memory-test rc 0 on spark (0.30 s) and on the Mac"
---

Problem: on spark with the Mac-built hb of the cross-OS stack (2026-11-11), bin/hb --load src/lib/memory-test.f prints three hex words (00000000f85f8e69, 0000000039400129, 00000000f84107fe) and ends rc 67 with 'hb: uncaught throw code -2502'. It passes on the Mac. Log: spark:~/carl-runs/xos4/o/red/memory-test.log. Seen first on 2026-11-05 at master bf05b860 (spark:/tmp/carl-linux/cmp-base2.log :27-37): the case RBT-RELEASE-FIRST (src/lib/memory-test.f:287, run as a child program `package MEMT RBT-RELEASE-FIRST ;package`, :346) dies with SIGSEGV (crash report sig 0xb) and the suite then reports a timeout, the -2502. Cause (hunt, 2026-11-11, strace on spark): the engine and memory.f are right. The child faults as the case intends and the crash handler re-raises SIGSEGV (src/habu/crash.f C-CRASH-RERAISE); spark's core_pattern pipes every such death to apport, which takes about 1.1 s, and the case allows 1000 ms (RBT-TIMEOUT-MS), so the parent reports a timeout. `prlimit --core=1 bin/hb --load src/lib/memory-test.f` passes; a limit of 0 does not skip a piped helper. The three hex words are the crash report's instructions at the intended fault. Fix: the test sets its own core limit to 1 before its cases, which its children inherit (brief carl-layout/.out/briefs/memory-test-no-core.md). Not checked: whether another test's expected fault meets a deadline on a host with a core helper (none was red on spark). Acceptance: the cause is named with path:line and fixed at its owner; the test passes on spark and on the Mac. Files: unknown until the cause. Verify: the test on spark. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-mem (hunt done; fix lane in the same workspace).
