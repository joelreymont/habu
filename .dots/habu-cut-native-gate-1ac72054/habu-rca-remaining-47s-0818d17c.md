---
title: RCA remaining 47s gate critical path
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-06-29T02:57:55.277500+02:00\\\"\""
closed-at: "2026-06-29T03:06:53.410088+02:00"
close-reason: "completed: implemented guarded early manifest/libs scheduling after proving HABU_UNDER_TEST is restored before early start; exact hot full gate passed at 43.206s internal / 46.09s wall with unchanged counts inner-hb=55 boundary=95 helper-spawn=106; rejected early artifacts regression remains documented"
---

Problem: after stdlib tool collapse, hot full gate is green at 44.811s internal / 47.81s wall, still above the 30s target. Latest measured tails: AOT-positive 31.686s, check-cli 25.359s, tool-boundary 24.961s, engine fixture 23.785s, late lint tools/libs/manifest 10.305s/8.267s/7.893s. Focused-only parallel split of tool-boundary-lints improved focused wall but regressed full hot due contention, so next work must be full-DAG RCA first. Fix: inspect test/run.f phase scheduling and gate stats, identify the actual critical path and duplicated launches after the current commit, then implement only a change that improves the hot full gate. Acceptance: before/after full-hot evidence, counts not worse without explanation, and rejected focused-only variants recorded.

Checkpoint 2026-06-29: Scheduler RCA found hot runs restore
`HABU_UNDER_TEST` before `TR-EARLY-START`, but `stdlib-manifest` and
`stdlib-lint-libs` were still serialized in the late wave. Accepted guarded
early scheduling for those two slices only when the under-test cache is already
ready; late scheduling skips the already-started phases. Full hot gate improved
from 44.811s internal / 47.81s wall to 43.301s internal / 46.32s wall with the
same process counts (`inner-hb=55`, `boundary=95`, `helper-spawn=106`).
Rejected: also starting `stdlib-lint-artifacts` early filled the early pool and
regressed to 43.676s internal / 46.70s wall, so artifacts remains late.
