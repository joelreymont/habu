---
title: "Keep the includer's usings across a buffer"
status: closed
priority: 2
issue-type: task
closed-at: "2026-10-01T13:11:59.126070+03:00"
close-reason: "acceptance (1) and (3) by a per-buffer using floor (engine, Habu loop, source verifier): using-test, outer-interpret 142 agree, clobber-lint clean; (2) split to habu-restore-the-includer-f7be16fa (the checker names mirror must follow the engine slots)"
created-at: "2026-10-01T12:34:32.155149+03:00"
---

Problem: a buffer can overwrite its includer's open usings. `INCLUDE-EVALUATE` restores only the using depth and floor when the buffer returns. A buffer that drops below its entry depth and then opens a using writes into the includer's slots, and the restored depth exposes the buffer's package where the includer's was. Measured on master 5c32 (lead repro): top level `using UA`, then `s" ;using using UB"` through `INCLUDE-EVALUATE` under `catch` returns 0; afterwards the top level resolves `BW` (22) and `AW` is `E-UNDEFINED`. The throw path (`s" ;using using UB NO-SUCH-WORDX"`) leaves the same slot. Inside `package PP` + `using UA`, a buffer `;package using UB` closes PP for the includer, whose own `;package` then fails (rc 75). Found by the PS lane (`habu-restore-pkg-state-d09394b9`).
Acceptance: a load file is a using scope (docs/forth.md "Importing ... with `using`": the scope ends at the end of the load file). (1) A buffer's `;using` that would close a using the buffer did not open is refused by name, as inside a package: `ENGINE-ERROR:USING-OUTER` (rc 104) in the engine and `E-USING-OUTER` (7146) in the source verifier. (2) On a caught throw, the restore puts back the includer's slots below the entry depth along with depth and floor (at most `USE-MAX` cells). (3) On a clean return after the buffer closed the includer's package, the depth is the one that `;package` restored; no slot the buffer wrote stays visible. Tests: the three repros above, as rejected or restored programs in the using suite, run through the real load path.
Files: the using depth/floor save and restore around `INCLUDE-EVALUATE` and file load (engine and `src/habu/outer.f`), the `;using` refusal, the source verifier's mirror, the using suite, `docs/forth.md` "Importing ... with `using`" and the card's using line.
Verify: the using and package suites; chain and full gate.
Depends: none.
Ownership: krait.
Claim: unassigned.
