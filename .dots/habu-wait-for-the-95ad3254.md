---
title: Wait for the post-answer prompt in every PTY fixture
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T14:37:04.517586+03:00"
---

Problem: a REPL child's line editor redraws 'habu> ' plus the line on every keystroke, so after SEND-LINE the buffer holds a prompt BEFORE the answer; a fixture that waits for the prompt with WAIT-FOR and then sends input (or ^D) races the child's cooked->raw switch (~100 us alone, milliseconds under load) and the input lands in cooked mode - a ^D becomes an EOF marker the raw read sees as NUL (measured by strace on test/repl-address-cell-rollback.f, fixed there with lib/pty-harness.f WAIT-AFTER). Acceptance: every fixture that sends input after a prompt that follows an answer uses WAIT-AFTER (answer, prompt) - test/aot-data-span-forge.f (lines ~205, 228, 281, 307, 342 wait for 'habu> ' after a magic marker), lib/pty-harness-test.f STOP-CHILD (127 SEND-BYTE then 'habu> ' WAIT-FOR: the 127 is echoed, so the prompt it waits for is the redraw), test/proc-pty.f and test/process-pty-tty-smoke.f where they use WAIT-FOR for a non-first prompt; ten runs each at load >= 10 green; no assertion weakened. Files: test/aot-data-span-forge.f, lib/pty-harness-test.f, test/proc-pty.f, test/process-pty-tty-smoke.f. Verify: each suite ten times under a bounded load driver. Depends: none. Ownership: lib/tests (alder). Claim: unassigned.
