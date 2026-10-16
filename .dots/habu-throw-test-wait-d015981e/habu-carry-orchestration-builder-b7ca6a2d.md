---
title: Carry orchestration builder deadlines
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.791195+03:00"
closed-at: "2026-10-16T23:08:27.792922+03:00"
close-reason: "Landed: the orchestration builder fixture names its release wait (gate-images-orch-builder WAIT-RELEASE) and exits 124 on its deadline; the registry wrapper carries E-PROC-TIMEOUT through SUITE-FINISH (now exported from test/gate-stdlib-lib.f) and dies PROC-TIMEOUT-RC; COMPILER-TIMED-OUT? reads the red pool's timed-out flag only when the row is not over its CPU budget, the rule GT-POOL-KIND-NAME. uses. Proofs: row gate-images-orchestration rc 0; forced release copy rc 67 with the named line and -2502, the compiler row labelled TIMEOUT-UNDER-LOAD; forced CPU-budget copy rc 1 kind=CPU-BUDGET with ordinary TFAILs, rc 67 before the fix. Astra review Medium (CPU-budget kills read as deadlines), fixed; Astra follow-up APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- `test/gate-images-orch-builder.f:18–24`: name expiry and exit 124 instead of 71.
- `test/gate-images-orch-registry.f:3–9`: install a fixture-local teardown wrapper after `STDLIB-GATE:MAIN`. Read the existing compiler-red timeout flag. For that pending timeout, call the normal `SUITE-FINISH` reporting/cleanup path with a nonzero pending code, then exit 124. Otherwise retain normal teardown behavior.
- `test/gate-images-test.f:67–107,127–141`: after saving captured logs and before success/failure assertions, recognize nested-gate exit 124 and rethrow through `SUBJECT:RC-TIMED-OUT`.

- `test/gate-stdlib-lib.f`: export `SUITE-FINISH`, which is private, so the registry's teardown wrapper can call it.

Do not change `test/compiler-images.f` or general gate exit policy.

**Comments**

Correct the serial-gate “fail promptly” descriptions at `gate-images-orch-builder.f:1–3` and `gate-images-test.f:1–5`: the bounded wait reports timeout.

**Proof**

Run `gate-images-orchestration`: `test/gate-images-test.f` (`gate-stdlib-cases.f:2485–2487`). In its private copied fixture, omit release-file creation and shorten the builder wait.

Verify builder 124, inner compiler row 67/−2502 and `TIMEOUT-UNDER-LOAD`, nested gate 124, then outer row 67/−2502 with the captured named builder line. Normal post-release failure still checks builder exit 73 and dependent exit 75 (`gate-images-test.f:109–124`).

**Acceptance**

The outer test never asserts an unexpected nested deadline as an exit-code mismatch. Logs are saved before propagation; normal cleanup still runs.

Files: test/gate-images-orch-builder.f, test/gate-images-orch-registry.f, test/gate-images-test.f, test/gate-stdlib-lib.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d13.
