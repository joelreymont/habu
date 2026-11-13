---
title: Bring the x86-64 profiler level with arm64's
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T07:26:14.195420+02:00"
---

Problem: the x86-64 profiler (src/habu/prof-x64.f) does less than arm64's (src/habu/prof.f, whose handler and control words spec/ProfClock.tla models): its stack walk stops at ARN-WALK (64 cells) and SP's 4 KiB block where arm64 walks to the main stack's top; it counts one per SIGALRM where arm64 weighs each sample by the whole intervals elapsed, carrying the remainder and a hold's time; it leaves samples inside C uncharged where arm64 charges them to the calling word; it counts a SIGALRM on any thread where arm64 counts only on the thread that ran prof-on and forwards the others to it; its control words neither wait for a running handler nor take arm64's ticket lock; and its prof-child-reset clears only the armed state. Acceptance: x86-64 does each as arm64 does, test/gate-debug-lib.f's profiler cases (GDB-PROFILER-FOREIGN, GDB-PROFILER-TASK) assert the same on both, and test/x86-64-kernel-prof.f (switched off in test/gate-stdlib-cases.f's x86-64 block; its HOST-SLOT! weight cell and CUT-FIELD header split were written for arm64's weights and have never run) passes, when x86-64 work resumes in its turn. Depends: none. Claim: unassigned.
