---
title: Bound the remaining CPU-heavy child deadlines by CPU
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T13:07:23.483940+02:00"
---

After 6d93453d (fc7763d3) suite and build rows end on their own CPU use, but rows still hold inner wall deadlines on CPU-heavy children that the 64-hog test would stretch past (review 434): test/gate-common-lib.f:19 GE-TIMEOUT-MS 120 s on hb-build in the two aot-positive rows (29 s CPU each), test/native-window-owner.f:38 180 s (48 s in a full pool), test/compile-floor-gate.f:73 180 s, tools/check-core.f CHK-DEADLINE-MS 120 s where a gate row runs check.f on a heavy subject, test/app-image.f:13 600 s (38 s CPU). Also the budget shape (360 s, x5, +60 s) is written three times: test/suite-budget.f, test/keyed-image.f:80-81, test/gate-images.f:270; one row-budget module should own it. Acceptance: each such deadline becomes SUITE-BUDGET:CHILD-MS or is shown by measurement (CPU per child, deadline/CPU ratio as for c2-init-accessor) not to be reached under the 64-hog test; one module owns the budget constants; the rows pass beside 64 hogs, bounded and reaped.
