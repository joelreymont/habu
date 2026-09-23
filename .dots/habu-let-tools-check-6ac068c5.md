---
title: Let tools/check.f see definer-generated names
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T22:02:26.305945+03:00"
---

tools/check.f preverify (VERIFY:SOURCE-BUF-IN-SCOPE, tools/check-core.f:1286) reads a source in scope without running its load-time INCLUDE-EVALUATE, so a name a codegen definer generates (CMD:COMMAND, TASK:+USER) refuses E-UNDEFINED unless the engine image already carries it: tools/check.f lib/process-command.f is rc 70, token CTX0 at line 436, on engine f2064116, and a 7-line TASK:+USER probe on a source the engine does not carry refuses identically; lib/process.f, lib/task.f and lib/fs-mutate.f pass only because their rows are baked. Before the CMD change the untouched module refused rc 67 / 7121 E-CHECKER-LAYOUT-BUFFER instead (TYPED-BUFFER counts given as constant names, the rule src/habu/verify-source.f:341 states; lib/queue.f refuses the same way), so no lib module with either shape is checkable today. Acceptance: verify-source.f models definers that evaluate generated source, or check.f runs the load-time definers in a scratch scope, so tools/check.f lib/process-command.f and lib/process-task-test.f are rc 0 and lib/queue.f is either rc 0 or refused with a diagnostic naming the count rule; a rejected-program row for a definer-generated name that is really undefined stays rc 70. Files: src/habu/verify-source.f, tools/check-core.f, tools/check.f. Verify: the two check.f runs, tools/check-core.f on the tree, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
