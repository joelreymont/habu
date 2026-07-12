---
title: "checker: nominal-value introduction is ungoverned (untyped-cell value laundering)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T00:09:56.917038+02:00"
---

TK-CELL capability scrutiny (2026-07-13): part 2 governs POINTEE identity, but value-level laundering remains expressible: '( n -- CAD-KIND:region ) VAR ! VAR @' still certifies - a fetch from an UNTYPED cell is a fresh var that binds a declared family OUTPUT in value position. Closing this generally = nominal values born only from constructors/validated refinements/trusted mints (a birth-certificate discipline for TK-CELL families), which interacts with every '( ... -- fam )' declared boundary in the tree. Needs design: probably a declared-output audit tier or a CUR-STRICT analog for value outputs of unchecked-provenance fetches. Evidence: probe in the TK-CELL review chain. Type-system lane; large blast radius - design dot first.
