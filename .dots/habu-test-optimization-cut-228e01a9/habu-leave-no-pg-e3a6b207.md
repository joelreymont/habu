---
title: Leave no pg row postmaster or socket on a kill
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-30T18:18:08.089921+02:00\""
closed-at: "2026-10-01T17:10:46.571980+02:00"
close-reason: Fixed by tyluslzq 5126e815 and polsrlvo 921e012b (reviews 57, 104 ACCEPT)
---

Problem: test/db/pg-cluster.f starts the cluster with pg_ctl, whose postmaster daemonizes (leaves its parent and its group), so the gate pool's process-tree kill (lib/process-tree.f, change monlstxr) cannot reach it. When the row is killed (pool deadline, or the gate root signalled), the row's own stop and cleanup do not run: the postmaster keeps running until its lock-file recheck notices the removed data directory, about a minute later, and the socket directory under TMPDIR stays for good. docs/db.md:312-320 and docs/gate.md:79-81 record this as a limit. Acceptance: a pg row killed by the pool (deadline or signalled root) leaves no postgres process and no socket directory, within the same bounded time as the other rows; an E2E case kills a live pg row that way and asserts both; docs/db.md and docs/gate.md state the new rule. Files: test/db/pg-cluster.f, test/gate-pool.f if the slot must own a short directory, docs/db.md, docs/gate.md. Verify: the new case, the pg rows, test/gate-signal-test.f. Depends: habu-end-a-signalled-14f5465b. Ownership: the pg harness's cluster lifetime. Claim: unassigned.
