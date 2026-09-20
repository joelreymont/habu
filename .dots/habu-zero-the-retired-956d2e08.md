---
title: Zero the retired checker symbol table after growth
status: active
priority: 2
issue-type: task
created-at: "2026-09-20T04:30:42.124581+03:00"
---

Claim: alder, .jj-ws/alder-symbol-retire, after coordinate normalization b4b544b4. Hazel released exactly SYM-GROW at checker.f:5214: after copying the old table to its grown allocation, zero the retired bytes before installing the new pointer. No marking retired rows. Measured native/profile sparse DATA differs only in all32766 SYMS-BOOT pointer fields (+16); dead table contributes202723 content bytes and143562 row bytes. Fixture: existing symbol growth asserts retired bytes zero; preserve engine-suite old-table restoration fixture. Proof: native/profile SHA equality, three generations, matrix, all AOT/stripped/whitebox rows on the final pair.

Repair: use the existing ARENA-CELLS-ZERO over the old table after its copy and
before storing the new SYMS-P. The existing growth fixture now checks nonzero
before and entirely zero afterward; new assert 139 fails on the old pin and
passes on the product. All prior lookup/index assertions remain. The engine
whitebox probe saves/restores its original table bytes because it deliberately
reinstalls that old pointer; its assertions also pass. Astra xhigh review clear.

Native and profiled products are byte-identical at SHA-256
e9d37e79bf90193f96231b3b5d1b6138eaa68ece18d2c1a72e5ea9d0e608913a.
The product is 4,063,424 bytes, down 393,216 from 4,456,640. Captured DATA span
is unchanged at 8,346,672 bytes; the sparse representation shrinks by 346,283
bytes (346,285 retired-table bytes minus two extra bytes for the next run gap).
No heuristic suppresses pointers and no retired rows are marked. The separate
coordinate-normalization matrix passes. All three native generations and the
profiled product have the SHA above. All 72 complete owning/reader/AOT/stripped/
whitebox registry rows passed, plus the matrix and check-only bootstrap.
build-fixpoint-fixtures invokes prohibited install --force and is deferred to
Hazel's serial gate. Evidence is in /tmp/alder-symbol-retire, with a private
host/tree/HOME/tmp. No full gate or shared-engine writes in this lane.
