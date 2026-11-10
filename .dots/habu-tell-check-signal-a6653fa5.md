---
title: Tell check-signal-subject to close its output without the environment
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.503802+02:00"
---

Problem: test/capture-tree-test.f:141 sets the environment variable CHECK_SIGNAL_CLOSED for the child test/check-signal-subject.f, which reads it at :31 to close stdout and stderr first (:7). A test child's setting can travel as a script argument or program text instead. Source, Joel 2026-10-29: remove env vars except where Habu really needs one and cannot do without it. Acceptance: the subject learns to close its output from its argument or program text, and CHECK_SIGNAL_CLOSED is gone. Files: test/check-signal-subject.f, test/capture-tree-test.f, test/check-signal-test.f. Verify: rg -n CHECK_SIGNAL_CLOSED prints nothing outside .dots; hb --load test/capture-tree-test.f and test/check-signal-test.f each rc 0. Depends: none. Ownership: unassigned. Claim: unassigned.
