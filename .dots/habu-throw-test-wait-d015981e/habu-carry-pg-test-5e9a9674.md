---
title: Carry pg-test progress deadlines as 124
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.889914+03:00"
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- `src/lib/pg-test.f:152–184`: name `CHECK-PROGRESS` expiry and throw `E-PROC-TIMEOUT`.
- Cover its later consumer at `EXIT-CASES:845–853`, not only connection/result polling.
- `MAIN:930–944`: preserve a pending timeout through cleanup and map it to child exit 124 at the case entry. Preserve other error behavior.
- `test/db/pg-cluster.f:220–232`: recognize exited 124 as a late failed step. Use existing first-failure state and server cleanup; `FAIL-ROW` already throws when late.

**Inferred cleanup hazard**

`CHECK-PROGRESS` also runs while AIO and an exit worker are active (`pg-test.f:845–848`). Its timeout can meet an `AIO:STOP` error in `MAIN`’s cleanup. Cleanup errors supersede body errors under `finally` (`docs/forth.md:1075–1077`). Finish the relevant worker cleanup, report any cleanup error, and preserve the pending deadline; do not let `E-AIO-BUSY` replace it.

**Comments**

Correct the “helpers run before AIO starts” description at `150–151`, which does not describe the later call at `848`. Document 124 as the case engine’s timeout outcome and retain the harness’s first-failure policy (`pg-cluster.f:212–218`).

**Proof**

Run the full `pg` row. Force a no-progress connection/result wait in one copied case and the exit-worker readiness wait in another. Verify case exit **124**, PostgreSQL stopped and reaped, then enclosing row `P(pg-test CHECK-PROGRESS)`.

**Acceptance**

Progress expiry is neither `E-EXEC`, ordinary child failure, nor cleanup-busy failure. Both default PG case files still run normally against one harness-owned cluster.

Files: src/lib/pg-test.f, test/db/pg-cluster.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
