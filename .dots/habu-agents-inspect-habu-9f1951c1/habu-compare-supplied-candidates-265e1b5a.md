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

Problem: nothing lets an agent hand in a few replacements for one span of a definition and learn which typecheck and which behave, so a type-correct wrong candidate is found only after it is accepted. In the REPL today: CHECK-CANDIDATE! (src/core/checker.f:25793) and CHECK-QUIET-CANDIDATE! (:25897) check a whole definition's text and print the stack mismatch; redefining a word is refused as a duplicate definition, so running a replacement means `undefine` (src/habu/xref.f:606 UNDEFINE-NAME) and a new definition; T= with T-REPORT reports a failed case. The word this dot adds must do more than that sequence. The mechanism is settled by the long-term rule (design /home/joel/Work/reviews/habu-10x-2026-11-10/designs/repl-queries-design.md sections 3.1-3.3): each candidate's whole definition and the fixture text run through the ordinary load (definer, `;`, compiler, checker) inside one evaluate frame that ends in a named throw, and the frame's rollback discards them (src/habu/interpreter.f:2656 ROWS-CUT, :2733 FRAME-ROLLBACK-CODE, :2776 FRAME-RECOVER, :2862 EVAL-FRAMED-RANGE, :2895 EVAL-FRAMED); CHECK-CANDIDATE! is not used. In-process only in the first version: a deadline needs a child, which inherits no session definitions, and src/lib/test/subject.f forks without exec. Refusals inside the evaluated text are placed wrongly until habu-an-evaluate-inside-d3da5e06 is fixed. The public word is not settled: proposed CHECK:REPLACEMENTS ( ptr u8 n n n ptr u8 n ptr u8 n -- ): definition text, span start, span end, replacements one per line, fixture text; printing per candidate `candidate N TEXT`, `check certified|rejected|uncheckable` with the load's diagnostic, `run completed|threw CODE|not run`, `case passed|failed|not run`; and a named discard throw, proposed CHECK:E-DISCARD.
Acceptance: first, the word's name, stack effect and output lines and the discard throw's name are decided by Joel. Then that word, for each replacement in the given order and with its id, splices it into a copy of the definition, loads the copy and the fixture text by the settled mechanism, and reports separately the check verdict with the load's diagnostic, whether the run completed or what it threw, and the case outcome read from the change in T-FAILURES: a passing check is not a passing case, a throw is not a type error. The fixture text is one argument for every candidate, so it cannot vary with the candidate; no candidate leaves a definition or a checker record behind; help says a fixture that may hang is unsupported. No candidate store, repair loop, planner or replay format.
Depends: habu-describe-a-published-501824c2, habu-make-incremental-builds-41fd3214.
Claim: unassigned.
