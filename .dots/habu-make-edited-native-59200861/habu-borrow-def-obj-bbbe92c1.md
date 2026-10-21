---
title: Borrow definition object sections
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T14:12:26.365039+03:00"
closed-at: "2026-10-21T14:24:51.358096+03:00"
close-reason: Import borrows validated immutable container sections through install/discard instead of copying every object. Real warm build avoids612829056copied bytes across13228hits; three bracketed pairs show~3.05%CPU/~2.74%wall gain, first inconclusive. Owning reads and format11 validation unchanged. Boundary and native-file-auto body/hook E2Es pass. Independent Astra xhigh CLEAR. Final actual rebuilt saved-support run13228hits/646compiled takes6.98wall/6.81CPU and reproduces exact67665a03engine/aa2b5d01map; no final-artifact shortcut.
---

Warm profiling attributes 2.8% to NUNIT-FILE:READ-BYTES-REUSE: every cached definition copies its complete immutable container payload into another arena before preparing sections. Trace the real PREPARE/INSTALL/DISCARD lifetime; if container bytes remain stable, read validated sections directly from that borrowed span without changing owning reads or external format validation. Measure the actual warm saved-support path against control with exact native executable/map parity; preserve malformed/truncated refusals and reset/failure cleanup. Keep only a useful measured simplification, with independent review.
