---
title: Throw filesystem task deadlines at the join
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.681646+03:00"
closed-at: "2026-10-16T23:09:18.209877+03:00"
close-reason: "Landed: fs-mutate-test's WAIT-FOR and fs-rename-noreplace-test's RACE-WAIT name themselves and throw E-PROC-TIMEOUT; JOIN-BOTH ( -- bool ) joins both workers, asserts only codes that are neither 0 nor E-PROC-TIMEOUT, answers true when both answered 0 and names and throws the deadline only when neither failed; all five *-RUN words exit before reading worker results after a failed join. Proofs: row fs-mutate rc 0; forced WAIT-FOR and RACE-WAIT copies rc 67 with one named line each and -2502 last; b-dies and a-dies copies rc 1 with two -2105 TFAILs and no -2502. Fable review REVISE (Medium: a genuine worker failure reported as the deadline), fixed with JOIN-BOTH; Astra follow-up APPROVE; a consequential peer deadline still asserted (the D6 review's finding) was fixed in the lead; second Astra follow-up APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- `src/lib/fs-mutate-test.f:1007–1021`: `WAIT-FOR` records `E-PROC-TIMEOUT`; `JOIN-OK` names and throws it. Preserve all other joined-result assertions, including consumers at `1050–1051,1068–1069,1084–1085,1148–1149`.
- `src/lib/fs-rename-noreplace-test.f:91–125`: `RACE-WAIT` records the timeout code; `WINNER?` names that code in main after the existing joins. Other errors continue to throw.

**Comments**

Clarify `fs-mutate-test.f:1013–1015`: the join rethrows a deadline; it does not assert that code against zero.

**Proof**

Run the complete `fs-mutate` row:

```sh
bin/hb --load src/lib/fs-mutate-test.f src/lib/fs-rename-noreplace-test.f </dev/null
```

Its registry is `gate-stdlib-cases.f:2497–2500`. Force a missed round-counter publication and, separately, a race rendezvous that never reaches two. Require `P(fs-mutate-test WAIT-FOR)` and `P(fs-rename-noreplace-test RACE-WAIT)` at main’s joined-result handling.

**Acceptance**

No worker deadline becomes an assertion or `E-FS-IO`. Existing filesystem outcome checks remain.

Files: src/lib/fs-mutate-test.f, src/lib/fs-rename-noreplace-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d7.
