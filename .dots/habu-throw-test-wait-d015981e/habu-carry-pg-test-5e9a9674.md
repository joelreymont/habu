---
title: Carry pg-test progress deadlines as 124
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.889914+03:00"
closed-at: "2026-10-17T00:36:22.628657+03:00"
close-reason: "Landed: pg-test CHECK-PROGRESS names its expiry (pg-test CHECK-PROGRESS) and throws E-PROC-TIMEOUT, so RUN exits 124 and pg-cluster reads 124 as late (exited 124, past its own deadline). STARTED-OR-HALT drops only E-TASK-NO-RESULT from the halted worker's join and prints any other code (pg-test EXIT-HALT: the halted worker threw <code>). LATE-STOP retries only E-AIO-BUSY; busy past PROGRESS-NS expires as pg-test LATE-STOP, and any other stop error is printed while the pending deadline stays the result. Proofs (macOS): row pg-cluster + pg-kill-test rc 0 in 8 s; forced copies exitworker, nohalt, clean-throw and stop-throw each end 124 and -2502 with the expected lines, no stray processes. Untested: Linux busy retry, gate pool, full gate. Review: Astra full review (High: stop codes replaced the pending deadline; Medium: STARTED-OR-HALT dropped every joined error), both fixed; Astra follow-up APPROVE."
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

Files: src/lib/pg-test.f, test/db/pg-cluster.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d19.
