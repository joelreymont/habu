---
title: "Fields: add shared schema arena"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:36.155644+02:00"
blocks:
  - habu-migration-core-records-77182600
---

Own the shared-field campaign outcome, not an implementation workspace. The sole
field authority already exists in `src/core/type-family.f`; no second arena or
package may be introduced. The remaining work is represented by the exact leaf
dots below: factor the schema validator, isolate its regressions, exhaust the
reserved-name table, then record visibility and stable provenance. This umbrella
closes only after those leaves and the later attach/retirement chain prove that
every STRUCTURE and ENUM consumer reads the one shared authority.

2026-07-20 substance verdict (fieldsroot lane): the deliverable already exists on master - the transactional PF arena in src/core/type-family.f with the sealed TYPE-FIELD reflection is the documented sole field authority (seal-reopen probe exit 84; suite/load/FILEMAP rows present). This dot historically remained active as the campaign umbrella, but it is now open and unclaimed pending its exact child dots; no new arena file may be created (a second field package would be a forbidden duplicate authority). The one-concern-per-file extraction is habu-extract-pf-field-e32bfbe1, sequenced after habu-fields-attach-variant.

Claim: RELEASED 2026-07-21. Parent umbrellas are never operationally claimed;
the former `fieldsroot` and `habu-fields-schema-v2` workspaces remain evidence.
