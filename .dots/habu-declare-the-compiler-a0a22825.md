---
title: Declare the compiler and AOT records
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:24:05.107825+03:00"
---

Problem: five src T3 records are persisted or captured and mix pointer and scalar fields: the NSTR pool owner (src/compiler/native/string.f:22-37, hand ptr-cell-mark), the IR arena descriptor (src/compiler/ir/arena.f:142-154, about 31 view sites), the IR symbol index (src/compiler/ir/symbol.f:306-316), the AOT dict record (src/habu/aot-closure.f:105-107,223, 48 bytes, AOT-captured), the address-cell vector header (src/habu/address-cells.f:159-195, the relocation machinery itself, base as a DATA offset). Acceptance per record: declared, accessors generated, marking from the declaration pinned for the captured ones, no cast left, byte fixpoint, its suite unchanged in what it asserts. Files: as listed. Verify: fixpoint; test/run.f. Depends: habu-generate-typed-field-ba63866e, habu-replay-derive-addr-5fd9a813, habu-mark-a-persisted-5d0993aa. Ownership: compiler. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
