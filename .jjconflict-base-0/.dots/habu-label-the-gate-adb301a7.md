---
title: "Label the gate's unlabelled assertions"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T20:04:31.776927+02:00"
---

test/gate-stdlib-cases.f (and siblings) carry unlabelled assertions; localising one failure cost three full build-fixpoint-test.f runs (seeda lane 2026-08-11 - the worker added labels to debug and reverted them to keep its diff honest). Add T-LABEL rows so a red names its case. Mechanical, low risk, high debugging value. Files: test/gate-stdlib-cases.f + siblings found the same way. Depends: none.
