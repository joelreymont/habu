---
title: Drive unbound storage guards from vectors
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T20:36:18.208683+02:00\""
---

Full context: the substrate vacuity audit found three code mutations that leave the storage/structure gates fully GREEN — gate gaps, not proof vacuity; the theorems are fine, the vector tables lack rows. (1) IR-ARENA:ABORT is never exercised: deleting the slot-retirement line leaves ir-storage-proof green; add an OP-ABORT step to the vector vocabulary in test/compiler/ir-storage-schema.f, drive it in ir-storage-cases.f, emit the matching obligation, so an index minted before an abort is refused on both sides. (2) IR-CTX:DEPTH-ROOM never exercised: raising its bound past DEPTH-MAX stays green; add a row nesting to DEPTH-MAX requiring E-IR-CTX-DEPTH at the next entry. (3) Five guards pinned by frozen text only, never behaviourally: ROLLBACK's foreign-mark generation compare, WIN-STARTS window separation, TILE-CK's operand-window arm, ARGS-CK's this-block arm — add a cross-arena rollback row, an operation-pool window row, and an ARGS-CK wrong-block row. BINDING GAPS sections in the three model files record the details.

Claim: agent=storerows workspace=.jj-ws/habu-drive-unbound-storage-30b07943
