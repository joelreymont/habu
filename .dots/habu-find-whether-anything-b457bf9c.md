---
title: Delete the site maps nothing reads
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:36:26.779107+01:00"
---

Problem: the address map, the call map and the x86-64 site rows that share their bytes (src/habu/layout.f CALLMAP-OFF, ADDRMAP-OFF, SITE-N-CELL, SITE-ROWS-OFF) are only written: the `callmap-set`, `addrmap-set` and `reloc-maps-clear` primitives (prims.f, habu1.f, kernel-x64.f), habu2.f MARK-SITE-AT, MARK-SITE and EMIT-ADDR-SITE, publish.f and link-band.f, and data-claims.f's claims. Their one reader, src/habu/sites.f, went with the AOT capture chain (habu-delete-the-aot-fb486263). Deleting the call map cascades through publish.f RELOC-EXTERNAL and linker.f HOST-CALLS.

Acceptance: a search (names fold case) finds no reader of the three; what nothing reads is deleted with its writers, primitives, bands, claims, comments and the tests that only exercise it, both arches together; the chain build reaches its fixpoint and the full suite passes.

Depends: none.

Claim: unassigned.
