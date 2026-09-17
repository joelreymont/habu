---
title: "Give the smoke test's absence claims a window"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T14:27:55.594272+03:00"
---

Problem: test/process-pty-tty-smoke.f has three whole-buffer absence claims with no window discipline ('s" 42" HAS? 0= TTRUE' at line 170 in PIPE-STOPS, 's" E-UNDEFINED" HAS? 0= TTRUE' and 's" non-certified" HAS? 0= TTRUE' at 235-236 in TTY-STACK-RECOVERS): a buffer the child has not answered into grants them, the bug class habu-give-reject-in-71cf8ab6 fixed in test/proc-pty.f with BARRIER / REJECT?. Acceptance: each claim reads a window a barrier closed (a marker the child prints past the point the rejected text could appear) and is refused without one; a fixture shows the unbarriered leg refused and the barriered leg catching text the child really printed; suite green ten runs at load above 20. Files: test/process-pty-tty-smoke.f. Verify: bin/hb --load test/process-pty-tty-smoke.f. Depends: none. Ownership: test pty harnesses. Claim: unassigned.
