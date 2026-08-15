---
title: Make schedule-lint enforce suite registration
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-13T20:23:22.327727+02:00\""
---

Claim: agent=lint-sweep workspace=.jj-ws/habu-lint-sweep

Found by the locals-scope lane: schedule-lint counts scheduled files but does NOT enforce that a new test file is registered - it stays 0-finding with both of a suite's registrations removed. The passing-is-not-scheduled rule currently rests on reviewers remembering to break an assertion and watch the gate dispatcher red. Make the lint red on a test/compiler/native-*.f (or the tree's test naming convention) present on disk but absent from the fork list. Files: tools (schedule-lint owner). Depends: none.

CONFIRMED + one blocking consumer (vintage audit 2026-08-15,
re-appended after the pool incident): the seven fork-only files
have NO documented single-tier reason (the proof suites' cases-only
placement IS documented at gate-stdlib-cases.f:721-727) - and
target-policy.f's missing SUITE row now BLOCKS b3dfa307's closure
(otherwise satisfied: 352 contracts / 60 policies proven).
Registering the seven + the lint direction fix is one lane;
b3dfa307 and this dot close together.
