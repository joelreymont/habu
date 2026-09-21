---
title: Keep the REPL rollback PTY fixture green under load
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T13:15:06.813508+03:00"
---

Problem: test/repl-address-cell-rollback.f (suite repl-address-cell-rollback) drives a tty child through the REPL and reaps it with a 20 s bound ('pty reap: no exit within ms 20000', then assert 18 fails); under a saturated pool it went red in chains DB (assert 9 while another full suite ran) and DK (load 11), and is green alone every time. A gate that reds on load is not measuring the code. Acceptance: the fixture waits for the REPL's own prompt/echo before each line and reaps on an idle-based bound (no output for N s) rather than a fixed 20 s wall clock from start, or the gate registers it in the serial (non-pool) group like the other tty fixtures if one exists; ten runs at load >= 10 green; the assertions unchanged. Files: test/repl-address-cell-rollback.f, test/repl-address-cell-probe.f, test/gate-stdlib-cases.f (only if regrouped). Verify: the suite ten times under a bounded load driver. Depends: none. Ownership: lib/tests (alder). Claim: unassigned.
