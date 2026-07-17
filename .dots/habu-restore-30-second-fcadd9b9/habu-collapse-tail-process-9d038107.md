---
title: Collapse tail process boundaries
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-17T16:19:15.139238+02:00\\\"\""
closed-at: "2026-07-17T19:45:39.130792+02:00"
close-reason: "Landed in b1f3cf00: tail-process keeps exactly 15 direct boundaries and 167 fork-isolated subjects with a 10s ratchet; focused tail 4.50s and full gate remains below 30s."
---

Full context: exact integrated run /private/tmp/habu-full-integrated finished 34.58s wall after validation batching, but GROUP: stdlib/tail-process remained the slowest group at 30317ms and the gate still reported 183 helper-spawn rows. The prior habu-collapse-stdlib-tool-3ee78309 dot closed with a 47.81s full gate and did not enforce the 30s or process-count contract. Cause: tail-process still treats semantic tool/library proofs as cold process boundaries and has no per-suite exec ratchet. Fix: measure each tail-process case at the shared process hooks, move catchable semantics into checked resident/fork-isolated subjects, retain direct exec only for explicit CLI/startup/PTY/signal/timeout contracts, and enforce a fixed per-group exec/time budget. Acceptance: tail-process <=10s on the calibrated macOS profile; every retained direct boundary is classified and tested; no coverage removal; combined full gate <=30s in three isolated runs; total exact execs <=25 with validation/runtime batching; owning stdlib, host-lint, filemap-lint, trust, and typed-local gates green.
