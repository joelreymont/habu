---
title: "Give x86 profiler control words arm64's admission handshake"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T13:01:15.561438+01:00"
---

Problem: src/habu/prof-x64.f has none of arm64's admission (src/habu/prof.f PROF-IN, the held state during QUIET, ON-BODY leaving the armed state before it rewrites the owner), so on x86 prof-on, prof-off or a report run on a thread other than the owner can race a running handler, and a forward can land in the middle of a report while armed. Acceptance: x86's control words wait for running handlers as arm64's do, and spec/README.md's ProfClock invariants (OnlyOwnerCounts, ForwardHandleStable) hold for x86's shape; a test that fails today shows the race. Depends: habu-forward-an-x86-d2c88960.
