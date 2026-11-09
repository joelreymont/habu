---
title: Compare supplied candidates against a fixed case
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T19:42:25.723217+02:00"
blocks:
  - habu-describe-a-published-501824c2
  - habu-make-incremental-builds-41fd3214
---

Problem: src/core/checker.f:27256 CHECK-CANDIDATE! checks one supplied source as a candidate with rollback and the retry loop, but nothing lets an agent hand in a few replacements for one span and learn which typecheck and which behave, so a type-correct wrong candidate is found only after it is accepted.
Acceptance: one public word takes a definition's source, one selected span and a short explicit list of replacements, checks each resulting whole definition in the same source context through CHECK-CANDIDATE!, and returns per candidate, in the given order and with its id: the existing verdict and diagnostics; then, for the candidates that check, the completion and predicate outcome of one named behavioural fixture run through its normal path (src/lib/test.f), the fixture's predicate fixed independently of the candidate source. Checking, execution completion and predicate are reported separately: a passing check is not a passing case, a timeout is not a type error. A rejected candidate leaves nothing in the next check. Compatible static checks share the process; a candidate whose effects need isolation runs in a child through the existing process command (src/lib/process-command.f) with the reason stated, never fork without exec, and a child inherits no ad hoc REPL definitions. No candidate store, repair loop, planner or replay format.
Files: the inspection module under src/habu/, src/core/checker.f only where CHECK-CANDIDATE! needs a public wrapper, docs/forth-card.md.
Verify: `: SQUARE ( i64 -- i64 ) DUP * ;` with `*`, `+` and `DUP` for `*`: `*` and `+` check, `DUP` fails for leaving an extra value; against the fixture 3 gives 9, `+` fails; SQUARE checks normally afterwards.
Depends: Describe a published word in the live REPL; habu-make-incremental-builds-41fd3214.
Ownership: the candidate word and fixture hookup. Claim: unassigned.
