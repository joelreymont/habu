---
title: check.f refuses a VALUE-RECORD field of type q
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.294989+02:00"
---

Problem: test/engine-suite.f under tools/check.f reports 'checker: bad value-record field type q' at its four VALUE-RECORD lines, on the parent too, while the file runs green under bin/hb: a check.f, checker or declaration defect for a q field. Acceptance: the cause found and fixed at its layer; check.f passes engine-suite.f. Files: tools/check.f, tools/check-core.f, src/core/checker.f value-record path, test/engine-suite.f. Verify: hb --load tools/check.f -- test/engine-suite.f rc 0. Depends: none. Ownership: the layer found. Claim: unassigned.
