---
title: "Fields: add shared schema arena"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T17:12:36.155644+02:00\""
blocks:
  - habu-migration-core-records-77182600
  - habu-core-promote-shared-ba859cbb
---

Own src/core/type-field.f, test/type-field-suite.f, the exact post-hook native and recovery load rows, and FILEMAP row. Add one transactional field record keyed by family, optional variant, and field name with schema root, cell/byte layout, alignment, and flags. Add typed reflection queries plus duplicate/reserved-name negatives. Validate focused load, filemap, host, and typed diff lints.

Claim: agent=fields-add workspace=.jj-ws/habu-fields-add-shared-6b063c62 base=3aa6ca76.

Frozen provider contract: sealed package TYPE-FIELD owns nominal linear tx and draft tokens plus nominal field-id values. Private friend operations are OPEN, START, SCHEMA, LAYOUT, SOURCE, ADD, COMMIT, ROLLBACK, MARK, and RESTORE; their staged builder keeps each effect shallow and ADD publishes only a fully initialized row. Public read-only operations are COUNT, FIND, FAMILY@, VARIANT?, VARIANT@, NAME (copies into caller storage), SCHEMA@, SLOT@, CELLS@, BYTE-OFF@, BYTE-SIZE@, ALIGN@, FLAGS@, VIS@, SOURCE@, and EACH. FIND returns option<field-id>; EACH passes nominal ids without exposing arithmetic or arena pointers. The schema value is a SCHEMA-ROOT pool index. Variant absence is an explicit boolean and requires stored variant id zero; presence validates the id. Flags bit 0 means public visibility and bit 1 means byte-addressable; all other bits reject. SOURCE stores an opaque nonnegative source id plus byte offset and length. Names are copied into the owner arena, canonical lowercase, and unique case-insensitively within the exact family/variant-presence key; one-character field names remain legal, while declaration and generated-operation keywords reject. Rows are append-only and declaration ordered; family/variant ranges are contiguous. Transactions nest strict LIFO; rollback atomically restores row, name, draft, and transaction watermarks. IDs remain valid only until their enclosing rollback. Snapshot identity is the ordered semantic row fields plus exact name bytes, never capacity, allocation address, or unused storage. No mutable arena pointer or universal raw-id cast is public. Later declaration, variant, product, value-record, snapshot, and compiler owners consume this surface without adding a second registry.

Exact private effects: OPEN ( -- tx ); START ( tx n bool n ptr u8 n -- tx draft ); SCHEMA ( tx draft n n n -- tx draft ); LAYOUT ( tx draft n n n n -- tx draft ); SOURCE ( tx draft n n n -- tx draft ); ADD ( tx draft -- tx field-id ); COMMIT ( tx -- ); ROLLBACK ( tx -- ); MARK ( -- mark ); RESTORE ( mark -- ). Exact public effects: COUNT ( -- n ); FIND ( n bool n ptr u8 n -- option<field-id> ); FAMILY@, VARIANT@, SCHEMA@, SLOT@, CELLS@, BYTE-OFF@, BYTE-SIZE@, ALIGN@, FLAGS@, and VIS@ each ( field-id -- n ); VARIANT? ( field-id -- bool ); NAME ( field-id ptr u8 n -- n ); SOURCE@ ( field-id -- n n n ); EACH ( R n bool n [ R field-id -- R ] -- R ). START validates family/variant ids through the sealed existing registries; SCHEMA validates the root-pool index; VARIANT@ throws on an absent variant.

Loader ownership: this worker alone adds the TYPE-FIELD rows to the existing post-hook declaration load/path/provided lists and all corresponding pin/build inventories. It must not edit hb-build composition behavior; any overlap in tools/hb-build-lib.f is integrated by the change-file controller after the modular-build lane is frozen.
