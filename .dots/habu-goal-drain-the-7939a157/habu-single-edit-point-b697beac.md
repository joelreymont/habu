---
title: Single edit point for gate-case manifests
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:21:32.735303+02:00"
---

test/candidate-validation-test.f hand-mirrors test/candidate-validation.f: exact case counts (a new suite bumps 28->32 in one file after adding rows in the other) plus duplicated PATH-PIN/DIRECT-PIN row text. The counts pin intent - keep that - but the mirror file is the wrong place. Design: move the intent witness next to the declarations - candidate-validation.f declares its cases AND its own expected-count constants adjacent to them (one file, one edit per new case), and the whitebox test asserts (a) declared counts match enumerated cases and (b) every declared path exists on disk, deriving the row text instead of duplicating it. Structural drift (someone deletes a case without touching the count beside it) still fails loudly. Serialize behind habu-retire-deftype-onto-07227854 stage A (it edits the same candidate-validation files).
