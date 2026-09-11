---
title: Finish the profiler and debugger tooling asks
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T00:23:54.339269+03:00"
---

Problem: the tooling half of habu-complete-queued-profiler-a5ce1ad4 (closed with the crash fix cb4fece2) is still open: profiler output names are not package-qualified so private names collide in the dump; there is no caller attribution or call chain; no stop command samples one phase without exiting; the debugger patches an unsupported breakpoint target before rejecting it. Acceptance: dump rows carry the package-qualified spelling; a caller-chain report or attribution mode; prof-off stops sampling and leaves the counters readable by prof-report; BP+ on an unsupported target fails before any code mutation, with a named refusal; each with a regression in test/gate-debug-lib.f. Files: src/habu/prof.f, the breakpoint emitters in src/habu/habu2.f (rowan-owned, handed as a commit id), test/gate-debug-lib.f, docs/debugging.md. Verify: the profiler and debugger cases through a driver over test/gate-debug-lib.f on a rebuilt engine. Depends: none. Ownership: hazel. Claim: unassigned.
