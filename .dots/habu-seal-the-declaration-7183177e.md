---
title: Seal the declaration callback table
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.555621+02:00"
---

CG-22. Declaration callback-table growth uses anonymous mmap storage (src/core/checker.f:11-17,58-64); callback cells are persisted through xt! and snapshot metadata stores cell-DATA without proving the cell lies in aligned DATA. A capacity-1 coordinator grown by a second registration records an XTCELL offset outside [0, DATA-SIZE). Boot survives only because exactly five participants fit five rows. Fix: this is a sealed closed-world participant table — delete dynamic growth and allocator state, change the static capacity explicitly when membership changes, and make xt!, writer, and loader reject non-aligned cells outside DATA before mutation.

Scout update (2026-08-05): the PRIMARY cited surface is src/core/declaration-transaction.f, which the original text never names — :27 E-PARTICIPANT-CAPACITY, :43 MAX-ROWS, :133-137 the five xt! stores, :139-142 TABLE-ARENA-GROW, :161-162 allocator/diagnostic install, :194-204 GROW-TABLE running ALLOCATOR@ execute. checker.f:11-17 (arena prose) and :63/:68 (ARENA-BYTES-GROW/REG-GROW1) are the growth machinery it delegates to. XTCELL band for the aligned-DATA half: layout.f:770-773, :697, :782; checker.f:5929. Capacity-1 admission still at declaration-transaction.f:149. Live participant count is a runtime fact — probe at claim time.
