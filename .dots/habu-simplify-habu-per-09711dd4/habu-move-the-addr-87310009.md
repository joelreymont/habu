---
title: Move the Python test harnesses into Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.172706+03:00"
---

Problem: two Python files carry test logic, against the CLAUDE.md rule. test/address-cell-storage-oom.py holds the whole test (prlimit, /proc reads, rc and stdout asserts) for the registry row at test/gate-stdlib-cases.f:2331; Habu FFI already sets rlimits (process-fork-test.f:129-132). test/host-io-image.py (225 lines, never registered) checks UDP and serial cleanup through three saved native image generations and fresh processes, using a PTY and a UDP peer; docs/swiftforth-task-api.md:61 cites it as coverage, but no gated test covers it. Acceptance: checked Habu tests make the same assertions (src/lib pty-harness.f and udp4.f supply the PTY and the peer); the host-io test is registered in the gate; both Python files deleted; the registry rows and docs/swiftforth-task-api.md updated. Files: test/address-cell-storage-oom.py, test/host-io-image.py, the new tests, test/gate-stdlib-cases.f, docs/swiftforth-task-api.md. Verify: the two rows (state the host; /proc is Linux-only). Depends: none. Ownership: listed files. Claim: unassigned.
