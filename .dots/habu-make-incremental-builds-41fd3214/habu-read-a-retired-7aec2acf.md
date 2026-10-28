---
title: "Read a retired record's data origin"
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T20:27:17.100557+02:00"
blocks:
  - habu-print-every-refusal-337830b1
---

Roots lane L7 (R4): FIRST-RECORD-XT (defer, checker.f:12073, retired at prefix-boundary.f:32) and CHECKER-VERIFY-PKG-DEPTH (checker.f:1285, retired at xref.f:843): a definer run reaching a retired record gets DATA-HASH 0 (tools/native-unit-dependency.f:946-948) because DATA-ORIGIN-RECORD (tools/native-file-reference.f:1189) uses NDICT REC-FIXED/REC-DEFER-CELL, empty for a retired record, and UNSETTLED? (:516) treats retirement as a vanished origin. 12+11 files first-reason. Claim: unassigned.
