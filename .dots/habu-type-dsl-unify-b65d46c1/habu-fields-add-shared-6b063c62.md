---
title: "Fields: add shared schema arena"
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:36.155644+02:00"
blocks:
  - habu-migration-core-records-77182600
---

Own src/core/type-field.f, its focused suite, load list, and FILEMAP row. Add one transactional field record keyed by family, optional variant, and field name with schema root, cell/byte layout, alignment, and flags. Add typed reflection queries plus duplicate/reserved-name negatives. Validate focused load, filemap, host, and typed diff lints. Claim: agent=fieldsroot workspace=.jj-ws/habu-fields-add-shared-6b063c62. (Previous claim agent=habu-fields-schema-v2 verified stranded 2026-07-20: its workspace no longer exists and no commits reference it; re-dispatched because this dot is the type-chain root that habu-fields-attach-variant and habu-type-declarations-shared build against.)

2026-07-20 substance verdict (fieldsroot lane): the deliverable already exists on master - the transactional PF arena in src/core/type-family.f with the sealed TYPE-FIELD reflection is the documented sole field authority (seal-reopen probe exit 84; suite/load/FILEMAP rows present). This dot stays active only as the campaign umbrella for its open children; no new arena file may be created (a second field package would be a forbidden duplicate authority). The one-concern-per-file extraction is habu-extract-pf-field-e32bfbe1, sequenced after habu-fields-attach-variant.
