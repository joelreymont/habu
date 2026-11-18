---
title: "Keep the engine's DATA cells from checked programs"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T13:54:33.379081+01:00"
---

Problem: data-base (src/habu/prims.f, a global row) answers a pointer to the whole DATA region, and with an engine cell offset a checked program stores into any engine cell: `: P15 ( n -- ) data-base NCOMP-DISPATCH:DECL-CELL + ! ;` and `: P14 ( ^u8 -- ) data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field ! ;` pass tools/check.f --verify-only, rc 0, on the installed hb, and overwrite the checker's declaration owner; so do, with no data-base in the program, `NCOMP-DISPATCH:DECL-CELL RESERVED-PTR-U8-CELL MY-DECL : P10R ( ^u8 -- ) MY-DECL ! ;` and `: P10D ( ^u8 -- ) NCOMP-DISPATCH:DECL-CELL POINTER-STORAGE:DATA-CELL ! ;` (both reach data-base inside src/core/pointer-storage.f). habu-checked-src-reads-4f721c9b's acceptance (no access outside the object its base names) does not cover this: the object data-base names is all of DATA. Acceptance: no checked program outside the engine packages that own a DATA cell can read or write it through data-base or a cell offset constant; all four probes are rejected under --verify-only and when they run. Depends: none. Claim: unassigned.
