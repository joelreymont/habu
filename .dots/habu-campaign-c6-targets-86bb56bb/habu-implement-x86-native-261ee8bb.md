---
title: Implement x86 native breakpoints
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.383912+03:00"
---

Problem: breakpoints on compiled words (src/habu/debug.f: BP+ one-shot, BP* persistent, BPN skip-count, BP- and BP.) plant an arm64 BRK #0 (debug.f:15, :135) and are baked on arm64 only (debug.f:3-4; tools/hb-build-core.f:163-172 RUNTIME-FILES loads runtime-arm64.f and debug.f). On x86-64, SIGTRAP goes to the crash handler (src/habu/boot-x64.f:803 in INSTALL-CRASH,), so a trap kills the process. A port exists only on parked commits never merged to master: 4407e672 (pwsnnpqn, "Resume x86 native breakpoints with INT3": an INT3 patch in debug.f BP-PATCH32 and a resuming SIGTRAP handler in boot-x64.f), recorded by d8984f19 and checked on the Mac at dfa5151b, on a line based at master's 52c7636b4b. Reported for that line: a review found nothing; gate-debug, prof-rewind and prof-index passed in one process on x86-64; the arm64 engine built and its two-generation build matched byte for byte. Left open then: "full integrated native census pending image provider", which names no dot. The PTY tier 1 matcher question is habu-resolve-pty-tier-5105302d. The x86-64 tests are disabled until habu-re-enable-the-959502b5; whether an x86-64 engine builds from master was not checked.
Acceptance: on an x86-64 Linux engine built from master, a one-shot, a persistent and a skip-count breakpoint on a compiled word each print habu-bp:, the pc and the stack top and resume as on arm64, and BP- and BP. behave as on arm64; arm64 is unchanged.
Files: src/habu/debug.f, src/habu/boot-x64.f, src/habu/layout.f, tools/hb-build-core.f.
Verify: first, on an x86-64 Linux host, bin/hb --load test/gate-debug.f; then the same on arm64.
Depends: none.
Ownership: unassigned. Claim: unassigned.
