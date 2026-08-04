---
title: "Reach the emitter's branch bound end to end"
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T14:54:04.697605+02:00"
---

test/compiler/native-migrate.f used to prove the emitter's E-A64EMIT-REACH by staging a callee 256 MiB from the caller through the production migration entry (MIGRATE-FAR-ENTRY). That address is no longer statable: NMIGRATE:CALLEE refuses an address that is not where the staged spelling's own word begins, so every callee address a migration sees is a real code address of this process and the Bl displacement always fits. The predicate keeps its exact edge test (test/compiler/native-a64ir.f, A64IR:B-FITS? at 2^25-1 and 2^25), but the emitter's own throw at src/compiler/native/emit.f BL-WORD has no assertion left. Restoring one needs a module built for it rather than a migration: a HIR word model declaring a callable at a far entry, a body that calls it, and the chain fixture with A64EMIT:PLACE-AT, in test/compiler/native-emit.f beside the shapes it already builds by hand. The three intra-routine uses of the same bound (B, CBZ, BCOND for a routine longer than 128 MiB) have never been reachable and are not part of this.
