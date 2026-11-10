---
title: Make every x86-64 boot-harness image exit 0 on pulsar
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T23:41:58.177170+02:00"
---

Problem: on the x86-64 peer (pulsar-ua-106), images built by the tests that load test/x86-64-boot-harness.f fail their last check, EXPECT-BALANCED (test/x86-64-boot-harness.f:87), instead of exiting 0. A first review saw them pass only when run from their own directory as ./image and fail by absolute path, with extra arguments, under env -i or from another directory (base syscalls image exit 21, spawn-fds images 30 and 24). A later run in a fresh mktemp directory found every image failing even as ./image (dir 30, dup 29, fcntl 29, fork 29, io 30, spawn 30, spawn-fds 30, spawn-fds-fail 24, syscalls 21; armed images 83), directly or under timeout, from a login shell or not, so the failure may not depend on invocation at all. The cause is not known. x86-64 builds are off until x86-64 moves onto the linker (DECISIONS.md:1641-1653), so this cannot be run on master now.
Acceptance: the cause is reported at file:line and fixed there; every boot-harness image (test/x86-64-boot-signal.f, test/x86-64-kernel-*.f, test/x86-64-link-records.f) exits 0 on pulsar, run as ./image, by absolute path and from another directory.
Files: test/x86-64-boot-harness.f, src/habu/boot-x64.f, src/habu/kernel-x64.f, the boot-harness tests.
Verify: on pulsar, after x86-64 builds are back, build each boot-harness image and run it three ways (./image, its absolute path, from another directory); each exits 0.
Depends: habu-re-enable-the-959502b5 (x86-64 tests re-enabled).
Ownership: unassigned. Claim: unassigned.
