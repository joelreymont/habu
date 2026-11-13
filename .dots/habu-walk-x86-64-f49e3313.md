---
title: Bring the x86-64 profiler level with arm64's
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T07:26:14.195420+02:00"
---

Problem: src/habu/prof-x64.f (:645-647, :1227) bounds its stack walk by ARN-WALK (64 cells) and SP's 4 KiB block, as arm64's did before dot habu-walk-a-profile-d56f7c32, so x86-64 inclusive counts fall short; it also counts one per SIGALRM where arm64 weighs each sample by the intervals elapsed (habu-weigh-each-profile-3f36929e), and leaves samples inside C uncharged where arm64 charges them to the calling word (habu-charge-foreign-profile-b621aeff). It also counts a SIGALRM delivered on any thread, where arm64 counts only on the thread that called prof-on. Acceptance: the x86-64 handler counts only on the owner thread, walks to the main stack top, weighs samples and charges foreign samples as arm64 does, test/gate-debug-lib.f GDB-PROFILER-FOREIGN asserts the same on both, and test/x86-64-kernel-prof.f (switched off in test/gate-stdlib-cases.f's x86-64 block; its HOST-SLOT! weight cell and CUT-FIELD header split were written for arm64's weights and have never run) passes, when x86-64 work resumes in its turn. Depends: habu-walk-a-profile-d56f7c32, habu-weigh-each-profile-3f36929e, habu-charge-foreign-profile-b621aeff. Claim: unassigned.
