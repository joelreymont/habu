---
title: Carry orchestration builder deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.791195+03:00"
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- `test/gate-images-orch-builder.f:18–24`: name expiry and exit 124 instead of 71.
- `test/gate-images-orch-registry.f:3–9`: install a fixture-local teardown wrapper after `STDLIB-GATE:MAIN`. Read the existing compiler-red timeout flag. For that pending timeout, call the normal `SUITE-FINISH` reporting/cleanup path with a nonzero pending code, then exit 124. Otherwise retain normal teardown behavior.
- `test/gate-images-test.f:67–107,127–141`: after saving captured logs and before success/failure assertions, recognize nested-gate exit 124 and rethrow through `SUBJECT:RC-TIMED-OUT`.

Do not change `test/compiler-images.f` or general gate exit policy.

**Comments**

Correct the serial-gate “fail promptly” descriptions at `gate-images-orch-builder.f:1–3` and `gate-images-test.f:1–5`: the bounded wait reports timeout.

**Proof**

Run `gate-images-orchestration`: `test/gate-images-test.f` (`gate-stdlib-cases.f:2485–2487`). In its private copied fixture, omit release-file creation and shorten the builder wait.

Verify builder 124, inner compiler row 67/−2502 and `TIMEOUT-UNDER-LOAD`, nested gate 124, then outer row 67/−2502 with the captured named builder line. Normal post-release failure still checks builder exit 73 and dependent exit 75 (`gate-images-test.f:109–124`).

**Acceptance**

The outer test never asserts an unexpected nested deadline as an exit-code mismatch. Logs are saved before propagation; normal cleanup still runs.

Files: test/gate-images-orch-builder.f, test/gate-images-orch-registry.f, test/gate-images-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
