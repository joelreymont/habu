---
title: "Anchor proc-pty's prompt waits to their answers"
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T14:27:55.584409+03:00"
---

Problem: test/proc-pty.f has ten bare 's" habu> " EXPECT' waits that the line editor's echo redraw satisfies before the child has evaluated anything (src/habu/repl.f REDRAW prints CR ESC[K 'habu> ' <line> per keystroke), the defect habu-make-the-smoke-d32d1717 fixed in test/process-pty-tty-smoke.f with PROMPT-AFTER!; EXPECT-OK and EXPECT-PROMPT (test/proc-pty.f:232,235) are dead with them. Acceptance: every prompt wait in proc-pty.f is ordered after the answer it waits for (the smoke test's FIND-AFTER / PROMPT-AFTER! shape, or the shared harness of habu-share-one-pty-b1d88b7c), a fixture reads the buffer and shows the echo's prompt precedes the answer while the barrier's follows it, the dead helpers are gone, and the suite is green ten runs at load above 20. Files: test/proc-pty.f. Verify: bin/hb --load lib/errors.f lib/process.f test/proc-pty.f. Depends: none. Ownership: test pty harnesses. Claim: unassigned.

Claim: alder, .jj-ws/alder-pty-prompts on 1afd910c. The shared harness is
already present but proc-pty still uses bare prompt waits and the two dead
helpers. Test-only slice: use the harness's ordered wait and pin echo-before-
answer versus the true completion prompt. No engine or library edits.

Implemented: PROMPT-READY? uses PTY-HARNESS:AFTER?; WAIT-PROMPT keeps reading
under one deadline. Every former bare prompt wait names its preceding answer.
Ctrl-C waits past the complete garbage echo; teardown evaluates a hex probe and
waits past its decimal answer before sending Ctrl-D. The dead EXPECT-OK and
EXPECT-PROMPT helpers are removed.

PTY-PROMPT-ORDER types $BEEF . without Enter, observes the complete echo, and
refuses that prompt before allowing evaluation. It then pins echo < answer <
completion prompt. Replacing only the new predicate with the former bare
IN-BUF? check gives failures13/15/16 on the captured echo-only buffer (rc1);
the ordered implementation exits0. Existing assertions keep their meaning.
The engine-runtime-regressions registry row (the sole caller/reader row) passes.
Ten additional whole proc-pty runs pass at measured 1-minute loads24.70--26.45;
private32-thread Habu load ends after140s. Astra review clear. Artifacts and
per-run load samples: /tmp/alder-pty-prompts. No engine/library edits or full gate.
