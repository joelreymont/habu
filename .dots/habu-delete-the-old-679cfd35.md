---
title: Delete the old emitter and its bridges
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.620705+02:00"
---

CG-17 + CG-19 + CG-20 + CG-21, the deletion half of phase 4. After the cut: delete COMPILE-EMIT and the old direct emitter; NMIGRATE and its staged/address-keyed facts (reproduced SIGBUS: DEFINE + FORGET-DEFS-FROM + three ordinary recompiles exits 134 even without NREACH); NREACH, redirection, call-site scanners, and workload redirection ceremony (reach.f:194-204 rewrites BL displacements without relocation ownership; SAME-WORD-CK at reach.f:142 is a case-insensitive tail comparison); the 128-row replacement log (LOG-FIND returns the oldest row, rows never retire, migrate-forget-migrate resolves through the dead row); and the CODE-RECLAIM bridge watchers (publish.f:420-426, clobber.f:230-236, inline.f:742-747 — registrations survive FORGET, table publicly exhaustible at xref.f:455-504). Any metadata the final compiler needs lives on authoritative live XREF/dictionary records and retires with them. Do not build a watcher lifecycle framework, a live-row registry, or any replacement history. Blocked by the cutover dot.
