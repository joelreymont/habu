---
title: "Let a TASK:+USER row keep its roles"
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T14:18:09.011298+03:00"
---

Problem: TASK:+USER publishes a concrete ptr n row, so src/lib/process.f (5661e1ab, 2026-09-17) and src/lib/net/tcp4.f cast the fd/pid/rc/len/off roles at every call instead of carrying them in the cell: about twenty cast sites in six files outside src/lib/process (test/gate-common-lib.f, src/lib/test/subject.f, tools/build-fixpoint.f, the three spawn layers), and the cell-alignment idiom '#USER 7 + $FFFFFFFFFFFFFFF8 and' is repeated in src/lib/task.f, src/lib/process.f and src/lib/task-test.f. Public effects are meant to keep their roles. Acceptance: TASK:+USER (or a sibling definer) mints a typed cell view per role, e.g. a row cell declared as fd answers ptr fd, and aligns a row start itself behind a named constant; src/lib/process.f and src/lib/net/tcp4.f rows carry their roles again and the call-site casts go; the src/lib/task.f header states the rule; process and tasking suites and test/run.f green. Files: src/lib/task.f, src/lib/process.f, src/lib/net/tcp4.f, src/lib/net/udp4.f, their tests. Verify: src/lib/task-test.f, src/lib/process-task-test.f, src/lib/net/tcp4-test.f, test/run.f. Depends: none. Ownership: src/lib/task.f. Claim: unassigned.
