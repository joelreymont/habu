---
title: Open a pty pair in one place for lib and the suites
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T15:45:22.421529+03:00\""
---

Problem: the /dev/ptmx unlock-and-name dance with its ioctl constants now lives in three lib modules and at least two suites: lib/pty.f PTY:OPEN (Linux arm only, :59-66), lib/process-pty-io.f IO-OPEN-PTY-LINUX / IO-OPEN-PTY-MACOS (:223-237, both arms, private), lib/pty-harness.f OPEN-PAIR-LINUX / OPEN-PAIR-MACOS (65dde13a, both arms, private), and private copies in test/gate-env-stdin-tty-test.f and test/app-image.f. A fix to one arm (the O_NOCTTY bit, a Darwin ioctl number, the slave path bound) has to be made five times and the Linux-only PTY:OPEN already lags the others. Acceptance: lib/pty.f owns one public pair opener with both target arms (master descriptor plus the slave path, E-PROC-HOST on an unknown target); lib/process-pty-io.f and lib/pty-harness.f call it and carry no TIOC constant; the two suites use lib/pty-harness.f (or the opener) and keep no copy; rg '/dev/ptmx' finds lib/pty.f and documentation only; the pty, process-pty and harness suites green. Files: lib/pty.f, lib/process-pty-io.f, lib/pty-harness.f, test/gate-env-stdin-tty-test.f, test/app-image.f. Verify: bin/hb --load lib/pty-harness-test.f; the process-pty and pty suites in test/gate-stdlib-cases.f; test/run.f. Depends: habu-share-one-pty-b1d88b7c. Ownership: lib pty family. Claim: agent=hazel-pty-reap workspace=.jj-ws/hazel-pty-reap.
