---
title: Run secret-test.f in a suite
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T19:23:39.872450+01:00"
---

Problem: src/lib/secret-test.f, the end-to-end test of the native desktop credential stores through the public SECRET words, is in no SUITE (test/gate-stdlib-cases.f:479 runs only secret-dir), while src/lib/net/model-test.f:4 says it covers the native stores; nothing runs it. Acceptance: a suite runs it on a host whose desktop store it can reach (the macOS login keychain), or it is deleted together with that claim. Depends: none. Claim: unassigned.
