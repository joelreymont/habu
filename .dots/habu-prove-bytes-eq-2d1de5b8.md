---
title: Prove BYTES-EQ equals byte-string equality
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T22:15:51.920804+02:00"
---

Full context: formal/Common/Interning.v instantiates the symbol table at plain byte equality and justifies that step in a comment rather than a proof — the one asserted step in the file. IR-SYM:BYTES-EQ (src/compiler/ir/symbol.f) compares BYTES>CELLS-many whole packed cells, which equals byte-string equality only because PACK-CELL is deterministic, every symbol starts on a fresh cell, tails are zero-padded, and the LENGTH is compared first in ROW-MATCH?. Model PACK-CELL and prove that cell-wise comparison decides equality of the underlying byte strings given the length check and the zero-padded tail. Acceptance: the comment is replaced by a theorem; Print Assumptions still reports Closed under the global context.
