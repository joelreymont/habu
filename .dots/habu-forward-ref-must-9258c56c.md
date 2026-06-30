---
title: Forward reference must fail closed
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T21:55:49.171937+02:00"
---

Problem: while adding GT-POOL-START-FORK, a call to GT-POOL-WAIT-FREE before its definition made loading test/gate-pool.f crash with a habu-crash register dump instead of an undefined-word diagnostic. Repro from this change before fix: load test/gate-pool.f with GT-POOL-START-FORK defined above GT-POOL-WAIT-FREE; checker/tools/check.f exited 70 with no text. Fix: compiler/load path must reject forward references with file/line/token diagnostics, not crash or silent rc. Acceptance: minimal fixture ': A B ;' on the same load/check paths reports undefined B and exits fail-closed; no native crash.
