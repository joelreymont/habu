---
title: Assoc helper C fast path
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-23T09:13:49.927960+01:00\\\"\""
closed-at: "2026-02-23T09:22:50.318452+01:00"
close-reason: "Rejected: C helper path gave no ReleaseFast win; keep Zig"
---

src/jit/backend.zig + build.zig + src/jit/*.c: replace remaining jitAssoc hot helper overhead with a C fast helper wired directly as primitive target; keep semantics/tests; rebench ReleaseFast assoc.
