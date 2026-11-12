---
title: Drop the host arms the target line makes dead
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:21:50.955112+02:00"
---

Problem: words such as src/lib/signal.f SIGUSR1 (:36-39) and src/lib/time-cpu.f THREAD-CPU-CLOCK (:33-36) end in 'E-PROC-HOST throw' for a host that is neither Linux nor macOS. src/lib/ffi-abi.f:28 states its targets with ?TARGET, so a file that reaches it through its requires cannot load on any other target, so in those files that arm never runs and E-PROC-HOST (src/lib/error.f:58) states a second time what the target line states. Acceptance: in every file that reaches ffi-abi.f, the words choose between the targets the line admits with no third arm; E-PROC-HOST is deleted if nothing else throws it; the chain build reaches its fixpoint and the suite passes. Depends: none. Claim: unassigned.
