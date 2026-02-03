---
title: Add JIT num_eq
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-03T22:21:51.219459+01:00\\\"\""
closed-at: "2026-02-03T22:23:24.945778+01:00"
close-reason: Implement Op.num_eq in JIT
---

Context: src/jit/jit.zig:129; cause: JIT lacks Op.num_eq, forcing blacklist + VM fallback; fix: add .num_eq lowering with fixnum fast path + rt.numEq slow path; add parity case using (= ...); deps: habu-parity-tests-9be195f5; verification: zig build test.
