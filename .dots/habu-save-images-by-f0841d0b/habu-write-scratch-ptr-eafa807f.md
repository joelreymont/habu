---
title: Write scratch pointer cells as null at capture
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:05.965415+02:00"
---

T1a. Problem: a scratch pointer cell (PTR-VARIABLE, PTR-U8-TABLE, PTR-U8-TABLE-VARIABLE in src/core/pointer-storage.f) joins no address-cell row, so capture copies whatever address it holds and the byte scan judges it by value. Acceptance: those definers register their cells as scratch in the address-cell registry (a scratch kind: a new row tag or a separate persistent list in src/habu/address-cells.f), capture writes 0 into every scratch cell, and the save check never reads one; a program that leaves an address in a PTR-VARIABLE saves, and the restored program reads NULL-PTR there. Files: src/habu/address-cells.f, src/habu/layout.f (SNAP-RELOC), src/core/pointer-storage.f, src/habu/aot-capture.f, the save check in src/habu/aot-common.f/aot-data-cell.f, src/core/cell-effects.f and verify-source.f if a definer's effect changes. Verify: brief-td-t1a.md. Depends: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-td1.
