---
title: Migrate competitive evidence reading and record
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T13:55:54.583842+02:00"
close-reason: "Landed as efd35043976e."
---

Wave D2 of the unified-type migration program. maki/competitive-evidence.f:96 SUMTYPE reading 0 (rd-at n unit / rd-na BENCH:absence - TWO payload cells on rd-at, name both from source) becomes full-mode payload ENUM; :102 PRODUCT evidence 0 (slot n) becomes STRUCTURE per precedent. Consumers untouched, suites run. Full recipe: calibrated verdict tables both trees, A1 test pattern, REFLECT registry pins (the shared helpers are landed - use REFLECT:, do not hand-roll; key per R7), FIELD-removal kills, non-zero discipline, name-cliff precheck. Acceptance: competitive-evidence suite + dependents + maki/test.f green; both diff lints; census verify identical (all full-form).
