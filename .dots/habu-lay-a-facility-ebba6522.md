---
title: "Lay a facility's lock when TASK:FACILITY makes it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T22:53:52.727148+02:00"
---

Problem: TASK:FACILITY (src/lib/task.f FACILITY) allots zeros, which is no valid mutex on macOS, so every user calls TASK:FACILITY-INIT once (log.f, ws.f, bearer.f, mcp.f, fd-lock.f at load; aio.f and curl.f lazily behind RING-READY and LOCK-READY flags; Tender, Zen and Maki too). Since FACILITY-INIT lays PTHREAD_MUTEX_INITIALIZER (74c5ad4a), the definer could lay it, as C's static initializer does, and FACILITY-INIT and the lazy flags would go. Removing FACILITY-INIT is a public API change its callers must follow: settle by the long-term rule and tell Joel before a worker starts. Acceptance: a facility is ready when made; no FACILITY-INIT; the lazy flags in aio.f and curl.f go. Files: src/lib/task.f and the callers named. Verify: task-test.f, spawn-test.f, aio and curl tests on the built hb. Depends: incremental builds 100% (Joel 2026-11-09). Ownership: carl. Claim: unassigned.
