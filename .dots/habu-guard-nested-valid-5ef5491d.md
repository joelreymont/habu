---
title: Guard nested-validation-rca-test for non-Linux hosts
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T18:50:38.992570+02:00"
---

Found by the schedule-lint disk audit (habu-make-schedule-lint-d9ca528d): test/nested-validation-rca-test.f is on disk and no runner reaches it. Its own assertions PASS on macOS - it prints 'test: ok' - and then tools/nested-validation-rca-core.f:136 dies rc 64 with '/proc process state requires Linux'. So it cannot be given a SUITE row as it stands: the row would red the gate on every macOS host. The work is to split or guard the host-dependent half (the /proc topology read) so the host-independent asserts run everywhere and the Linux-only part skips rather than dies, then register it. tools/nested-validation-rca-core.f also requires HABU_GATE_STATS (line 198). Carries a schedule-lint: allow-unscheduled pragma naming this dot until then. Files: test/nested-validation-rca-test.f, tools/nested-validation-rca-core.f, test/gate-stdlib-cases.f. Depends: none.
