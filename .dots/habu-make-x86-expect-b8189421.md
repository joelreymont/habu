---
title: Make every x86-64 boot-harness image exit 0 on pulsar
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T23:41:58.177170+02:00"
---

Problem: on the x86-64 peer (pulsar-ua-106), images built by the tests that load test/x86-64-boot-harness.f fail their last check, EXPECT-BALANCED (test/x86-64-boot-harness.f:87), instead of exiting 0. Run in a fresh mktemp directory, every image failed even as ./image (dir 30, dup 29, fcntl 29, fork 29, io 30, spawn 30, spawn-fds 30, spawn-fds-fail 24, syscalls 21; armed images 83), directly or under timeout, from a login shell or not; an earlier run saw them pass only as ./image from their own directory. The cause is not known.
Acceptance: the cause is reported at file:line and fixed there; every boot-harness image (test/x86-64-boot-signal.f, test/x86-64-kernel-*.f) exits 0 on pulsar, run as ./image, by absolute path and from another directory.
Depends: habu-re-enable-the-959502b5 (x86-64 tests re-enabled).
Claim: unassigned.
