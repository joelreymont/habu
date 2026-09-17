---
title: "Let a TASK:+USER row keep its roles"
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T14:18:09.011298+03:00"
---

Problem: TASK:+USER publishes a concrete ptr n row, so lib/process.f (5661e1ab, 2026-09-17) and lib/net/tcp4.f cast the fd/pid/rc/len/off roles at every call instead of carrying them in the cell: about twenty cast sites in six files outside lib/process (test/gate-common-lib.f, lib/test/subject.f, tools/build-fixpoint.f, the three spawn layers), and the cell-alignment idiom '#USER 7 + $FFFFFFFFFFFFFFF8 and' is repeated in lib/task.f, lib/process.f and lib/task-test.f. Public effects are meant to keep their roles. Acceptance: TASK:+USER (or a sibling definer) mints a typed cell view per role, e.g. a row cell declared as fd answers ptr fd, and aligns a row start itself behind a named constant; lib/process.f and lib/net/tcp4.f rows carry their roles again and the call-site casts go; docs/threads.md states the rule; process and tasking suites and test/run.f green. Files: lib/task.f, lib/process.f, lib/net/tcp4.f, lib/net/udp4.f, their tests, docs/threads.md. Verify: lib/task-test.f, lib/process-task-test.f, lib/net/tcp4-test.f, test/run.f. Depends: none. Ownership: lib/task.f. Claim: unassigned.
