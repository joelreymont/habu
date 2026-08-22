---
title: "capture-only spawn path inherits the gate's stdin"
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.062371+02:00"
---

Problem: lib/process.f:408-414 PROC-SETUP-CAPTURE-FDS opens no stdin pipe, PROC-IN-R stays -1 (:326), src/habu/habu1.f:1005-1008 skips the dup2 for a negative fd, and the engine reads its second stream from fd 0 (habu2.f:1497-1498), so every child through STDLIB-GATE:SUITE-RUN-ENV (test/gate-stdlib-lib.f:164-172) and GT-RUN (lib/test/runner.f:133-136) - 72 files - inherits the gate's fd 0; GE-RUN-ENV (test/gate-common-lib.f:122-131) is the correct shape; test/run-lib-test.f:7-8 documents the symptom. Also test/proc-pty.f:12-14 polls 200x10 ms fixed; the Linux-only pty cases (aot-data-span-forge.f:405, gate-env-stdin-tty-test.f:143) have never run outside DGX Spark. Existing dot habu-stdin-inheriting-runner-ae3b87f6 names the class. Acceptance: the capture path opens an empty stdin pipe by default (explicit inheritance opt-in); GT-POOL-START verified for fd 0; a test spawns through each entry with a non-empty stdin and shows the child sees EOF. Files: lib/process.f, lib/test/runner.f, test/gate-stdlib-lib.f, test/gate-pool.f. Verify: the new test; proc-pty class reruns on this host. Depends: none. Ownership: process. Claim: unassigned.
