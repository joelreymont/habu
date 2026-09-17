---
title: "Anchor proc-pty's prompt waits to their answers"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T14:27:55.584409+03:00"
---

Problem: test/proc-pty.f has ten bare 's" habu> " EXPECT' waits that the line editor's echo redraw satisfies before the child has evaluated anything (src/habu/repl.f REDRAW prints CR ESC[K 'habu> ' <line> per keystroke), the defect habu-make-the-smoke-d32d1717 fixed in test/process-pty-tty-smoke.f with PROMPT-AFTER!; EXPECT-OK and EXPECT-PROMPT (test/proc-pty.f:232,235) are dead with them. Acceptance: every prompt wait in proc-pty.f is ordered after the answer it waits for (the smoke test's FIND-AFTER / PROMPT-AFTER! shape, or the shared harness of habu-share-one-pty-b1d88b7c), a fixture reads the buffer and shows the echo's prompt precedes the answer while the barrier's follows it, the dead helpers are gone, and the suite is green ten runs at load above 20. Files: test/proc-pty.f. Verify: bin/hb --load lib/errors.f lib/process.f test/proc-pty.f. Depends: none. Ownership: test pty harnesses. Claim: unassigned.
