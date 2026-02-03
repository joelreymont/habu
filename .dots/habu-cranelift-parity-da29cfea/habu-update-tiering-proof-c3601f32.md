---
title: Update tiering proof
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-02-03T22:19:11.113772+01:00\\\"\""
closed-at: "2026-02-03T22:20:11.968025+01:00"
close-reason: Point docs at jit_tiering test
---

Context: docs/cranelift-parity.md:29; cause: tiering row proof should point to dedicated hot-threshold test; fix: update Proof to src/tests/jit_tiering.zig test "jit tiering hot threshold"; deps: habu-test-jit-tiering-198b18e1; verification: zig build test.
