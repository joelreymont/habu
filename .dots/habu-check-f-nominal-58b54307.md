---
title: check.f nominal scan takes a row name for a declarer
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T18:09:44.262372+02:00"
---

Problem: bin/hb --load tools/check.f -- test/gate-stdlib-cases.f exits 70 'check.f: bad nominal type test/deftype-suite.f': the nominal scan matches the row name 'deftype' in 'WHITEBOX-SUITE deftype' (SUITE deftype on master) case-insensitively as the DEFTYPE declarer. Pre-existing on master. Acceptance: the scan takes only a declarer token in declaration position; check.f on gate-stdlib-cases.f rc 0; a check-test case with a lowercase non-declaration occurrence. Files: tools/check*.f nominal scan (rg 'bad nominal type'). Depends: none. Claim: unassigned.
