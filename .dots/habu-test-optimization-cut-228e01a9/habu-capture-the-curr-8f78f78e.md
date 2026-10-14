---
title: "Capture the current run's bytes in GE-CAPTURE-ACTION"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T12:03:31.926443+02:00"
closed-at: "2026-10-15T01:13:03.813019+03:00"
close-reason: "Completed by master ancestors 04273828 and 55146a28: capture uses per-action files, nested capture is reentrant and in-process diagnostics storage case is restored. Commits record focused green checks; current capture owner verified."
---

Problem: an in-process CHECK-JSON (test/gate-diagnostics-lib.f:~203 -> CHECK-CAPTURE -> test/gate-common-lib.f:~502 GE-CAPTURE-ACTION, at r4-tbuf dc825869) whose verdict comes from a run it spawns captured the right byte count (282) but the previous capture's bytes (evidence: $HOME/.cache/tmp/kestrel-r4-tbuf/gd-green.log). The r4-tbuf worker worked around it by running fixture RUN-STORAGE in its own process (GE-RUN-ENV). Acceptance: reduce the failure to the responsible word (capture buffer reuse, length vs content source, or the spawned run's output path), fix it there; a case that fails first shows stale bytes through the real gate-common load path; RUN-STORAGE returns to the in-process CHECK-JSON form and its workaround comment goes. Files: test/gate-common-lib.f, test/gate-diagnostics-lib.f and the owning test. Verify: test/gate-diagnostics.f rc 0 plus the gate-common tests.
