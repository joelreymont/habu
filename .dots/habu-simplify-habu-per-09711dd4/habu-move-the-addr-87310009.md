---
title: Move the address-cell OOM test into Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.172706+03:00"
---

Problem: test/address-cell-storage-oom.py holds the whole test (prlimit, /proc reads, rc and stdout asserts) for the registry row at test/gate-stdlib-cases.f:2331. Habu FFI already sets rlimits (process-fork-test.f:129-132). Acceptance: a checked Habu test does the same with the same assertions; the Python file deleted; the registry row updated. Files: test/address-cell-storage-oom.py, the new test, test/gate-stdlib-cases.f. Verify: that row (Linux-only if it reads /proc; state the host). Depends: none. Ownership: listed files. Claim: unassigned.
