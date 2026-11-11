---
title: Delete the range interpreter words DECISIONS.md drops
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T14:30:15.777504+02:00"
---

Problem: Joel's choice 'Drop it; tests use loads': INCLUDE-EVALUATE-RANGE, INTERPRETER:INTERPRET and INTERPRET-RANGE go, and tests run the Habu loop over file loads through SOURCE-ROOT:USE-INTERPRETER. All three still exist: src/habu/interpreter.f:3024-3081 (INTERPRET-OPEN, RANGE-A..RANGE-END, INTERPRET-STAGED, INTERPRET-CLOSED, INTERPRET-RANGE, INTERPRET), src/core/include.f:1909-1943 (EVAL-A..EVAL-END, EVAL-STAGED, CLOSE-RANGE, RUN-EVAL-RANGE) and :2178-2179 (INCLUDE-EVALUATE-RANGE), with checker.f:12778 (PE-SYM-OF row), :18356 (NORET-AXIOM) and :18336 (comment). One caller: test/c2-memory-entry.f:29-31, which runs INTERPRETER:INTERPRET over a string. Acceptance: the words and their only-used-by state are gone; comments naming them (interpreter.f:3, :250, :621, :2100, :2103; habu1.f:4053) say what is true; the c2-memory-entry case runs its text through a file load with the Habu loop switched on and still sees the refusal. Files: src/habu/interpreter.f, src/core/include.f, src/core/checker.f, test/c2-memory-entry.f. Verify: engine hb1=hb2; test/c2-memory-entry.f, test/interpreter-loop-on.f rc 0. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/interp-range.

State 2026-11-11 (jerry): the review's fold-ins are in xzlrqqzu c00a222b (lead's comment cuts plus RUN-CAUGHT, RUN-CLOSED, source-unit-run and B-SOURCE-UNIT-RUN deleted, B-UNIT-RUN folded to the file case); gate hb1=hb2 and engine-suite, c2-memory-entry, interpreter-loop-on, file-objects, load-reject-diag-test rc 0. Fix re-review running (brief .jj-ws.noindex/jerry-wasm/.out/briefs/review-interp-range-fold.md).

State 2026-11-11 (jerry): re-review of the fold REVISE: comment findings 1, 2, 4-7 applied by the lead as given; item 8 done by the lead (B-UNIT-RUN renamed B-SOURCE-FILE-RUN, its alias gone; x86-64 SOURCE-UNIT-RUN, renamed SOURCE-FILE-RUN,). Claim 3 (SOURCE-UNIT:GUARDED still wraps unit-compile-run, untested) goes with the dead guard: dot habu-delete-the-selected-d2339e3c, stacked on this lane. The machines' different source-file-run stacks: dot habu-run-src-file-552e98ae. Rebased onto master 2190b71f, xzlrqqzu e5feb2c1.
