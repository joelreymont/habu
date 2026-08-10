---
title: Relocate code addresses held in mmap payloads
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:56:46.272831+02:00"
---

Class 2 from the seed-closure enumeration (2026-08-11), deliberately UNDECIDED by that lane: clobber's R-ENTRY and publish's LOG-OLD/NEW-START columns hold routine code addresses INSIDE anonymous mmap payloads, which the declared-address-cell mechanism cannot reach (XTCELL-OFF-MAX requires cells inside DATA; layout.f:832 records the 4.6e12-below-data-base precedent). Two candidate routes, BOTH need a probe before choosing: (a) move those columns back to create/allot DATA declared with xt! - reintroduces the fixed ceiling the vectors were grown to remove; (b) extend SNAP-RELOC to declared spans outside DATA - a real format change. Choosing without the probe is the value-heuristic move the review gate exists to catch. Gates Stage D's snapshot builder for chain-published state. Files: probe first; then src/compiler/native/{clobber,publish}.f or src/habu/ SNAP-RELOC. Depends: none. Blocks: full snapshot support of chain-published routines.
