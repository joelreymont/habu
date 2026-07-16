---
title: "Fields: add shared schema arena"
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:36.155644+02:00"
blocks:
  - habu-migration-core-records-77182600
---

Own src/core/type-field.f, test/type-field-suite.f, the exact post-hook native and recovery load rows, and FILEMAP row. Add one transactional field record keyed by family, optional variant, and field name with schema root, cell/byte layout, alignment, and flags. Add typed reflection queries plus duplicate/reserved-name negatives. Validate focused load, filemap, host, and typed diff lints.

Claim: agent=sol workspace=.jj-ws/habu-fields-schema-clean base=75d2dc79.

Frozen provider contract: sealed package TYPE-FIELD owns nominal linear tx and draft tokens plus nominal field-id values. Private friend operations are OPEN, START, SCHEMA, LAYOUT, SOURCE, ADD, COMMIT, ROLLBACK, MARK, and RESTORE; their staged builder keeps each effect shallow and ADD publishes only a fully initialized row. Public read-only operations are COUNT, FIND, FAMILY@, VARIANT?, VARIANT@, NAME, SCHEMA@, SLOT@, CELLS@, BYTE-OFF@, BYTE-SIZE@, ALIGN@, FLAGS@, VIS@, SOURCE@, and EACH. FIND returns option<field-id>; EACH passes nominal ids without exposing arithmetic or arena pointers. Names are copied into the owner arena, canonical lowercase, and unique case-insensitively within the exact family/variant-presence key. Rows are append-only and declaration ordered; family/variant ranges are contiguous. Transactions nest strict LIFO; rollback atomically restores row, name, draft, and transaction watermarks. Snapshot identity is the ordered semantic row fields plus exact name bytes, never capacity, allocation address, or unused storage. No mutable arena pointer or universal raw-id cast is public.

Exact private effects: OPEN ( -- tx ); START ( tx n bool n ptr u8 n -- tx draft ); SCHEMA ( tx draft n n n -- tx draft ); LAYOUT ( tx draft n n n n -- tx draft ); SOURCE ( tx draft n n n -- tx draft ); ADD ( tx draft -- tx field-id ); COMMIT ( tx -- ); ROLLBACK ( tx -- ); MARK ( -- mark ); RESTORE ( mark -- ). Exact public effects: COUNT ( -- n ); FIND ( n bool n ptr u8 n -- option<field-id> ); FAMILY@, VARIANT@, SCHEMA@, SLOT@, CELLS@, BYTE-OFF@, BYTE-SIZE@, ALIGN@, FLAGS@, and VIS@ each ( field-id -- n ); VARIANT? ( field-id -- bool ); NAME ( field-id ptr u8 n -- n ); SOURCE@ ( field-id -- n n n ); EACH ( R n bool n [ R field-id -- R ] -- R ). START validates family/variant ids through the sealed existing registries; SCHEMA validates the root-pool index; VARIANT@ throws on an absent variant.

Loader ownership: this change alone adds TYPE-FIELD to the existing post-hook declaration load/path/provided lists and corresponding pin/build inventories. It does not change hb-build composition behavior.
