---
title: Fix already-dead race in proc-watch smoke
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-23T12:06:17.042103+02:00\""
---

Why: test/proc-watch-smoke.f red the gate-stdlib pool once under load (suite proc-watch-primitive-smoke, assertion 9) and passed 5 of 5 solo reruns - a genuine race in the TEST, pre-existing on master. DEAD-WATCH-LINUX asserts the watch descriptor is readable with a ZERO-timeout poll, gated only on alive-pipe end-of-file; but the kernel closes the exiting child file descriptors (which produces the EOF) before it completes exit notification (which makes the pidfd readable), so EOF proves exit STARTED, not that the child is already waitable, and under load the window between the two is routinely observable. This is a timing heuristic where a structural invariant is possible. Behavior: before opening the watch in the already-dead path, wait boundedly until the kernel actually reports the child as a zombie (state Z in /proc/pid/stat, or the equivalent structural probe), THEN open the watch and keep the strong zero-timeout readability assertion - preserving the intent of the case (pidfd open on an already-exited process is immediately readable) instead of diluting it into the bounded fast-path check. Owner: package PROC-WATCH-SMOKE in test/proc-watch-smoke.f; if the zombie-state probe needs a primitive the process library lacks, that capability gets its own dot rather than host glue. Acceptance: red witness reproduced by inserting a bounded artificial delay between file teardown and notification observation (or by running the pool under load until the flip reproduces), green after the structural wait; the fast-path and invalid-pid cases unchanged; the suite green solo and in the pooled slice; both lints exit 0.

Claim: agent=procwatch workspace=.jj-ws/habu-fix-already-dead-ba80de50
