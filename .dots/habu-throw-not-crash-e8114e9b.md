---
title: Throw, not crash, at a build-time call of a host-only function in a cross build
status: open
priority: 2
issue-type: task
created-at: "2026-11-28T08:31:02.341284+01:00"
---

Problem: building a file for another target that calls a FUNCTION: bound only on that target at its top level segfaults the build (rc 139) instead of rejecting the call. Reproducer: on Linux aarch64, from a Habu tree root, bin/hb --load tools/hb-build.f -- --target macos-aarch64 /tmp/tim-habu-xbuild/mac/test-entry.f -o /tmp/x (test-entry.f wraps src/lib/process-cpu-test.f, whose top-level RUN calls PROC-CPU:CPU-NS, bound to macOS's proc_pid_rusage). Under --load the same unresolved symbol throws E-FFI-DLSYM cleanly (/tmp/tim-habu-xbuild/probe/missing.f, rc 85). Found by tim's lane pcpu, 2026-11-28; cause not traced. Acceptance: the cross build rejects the build-time call with a named error at the call, rc not 139. Depends: none. Claim: unassigned.
