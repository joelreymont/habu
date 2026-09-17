---
title: "Select the signal-stub suite's numbers per target"
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T18:28:00.088484+03:00"
---

Problem: test/signal-stub.f (SUITE signal-stub) spells 10, 12 and 152 as bare Linux constants for the signals it installs and raises (aspen 2026-09-17, while lib/signal.f and lib/signal-test.f select SIGUSR1/SIGUSR2, SA_RESTART and the sigaction layout per target behind HB-TARGET-LINUX?/HB-TARGET-MACOS?); the suite is not red on macOS because it raises what it installs, but under the SIGUSR names it would be exercising SIGBUS and SIGSYS there. Acceptance: the suite takes its signal numbers from lib/signal.f's SIGUSR1/SIGUSR2 (or selects its own per target with a third target refused by E-PROC-HOST like the library), and any number with no macOS meaning is named for what it is. Files: test/signal-stub.f. Verify: bin/hb --load test/signal-stub.f; the gate. Depends: the signal lane landing (b5f8ee2f + 8b1050a7). Ownership: signal tests. Claim: unassigned.
