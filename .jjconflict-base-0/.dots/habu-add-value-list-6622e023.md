---
title: Add value-list attribute kind
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T21:50:50.344286+02:00"
---

Full context: design section 6.3 line 472 names a value-list attribute over ir-value-id references. src/compiler/ir/attr.f reserves wire code 9 for it and implements nothing (a forgery fixture currently pins the reserved code as un-decodable). Add the kind with the same staged-list pattern the int-list and record kinds use, once the value pool exists. Includes per-element owner and bound validation, render spelling, and identity fixtures. Dependency: the operation/value pools. Acceptance: identical value lists intern to one id; a foreign or not-yet-defined value rejects named; the reserved-code forgery fixture is replaced by a real decode test.
