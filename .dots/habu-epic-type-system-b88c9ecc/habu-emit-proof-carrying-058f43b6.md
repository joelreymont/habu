---
title: Emit proof-carrying register allocation certificates
status: closed
priority: 3
issue-type: task
created-at: "\"2026-07-13T11:44:22.408382+02:00\""
closed-at: "2026-07-31T17:39:45.191526+02:00"
close-reason: "Subsumed by A64RA/A64RAV (c19801e794c3): separate validator re-derives and rejects every listed mutation in-process; serialized certificate has no consumer; residue split into the spills and arg-binding dots"
---

Compiler-IR reconciliation: this dot owns the native register-allocation witness and independent validator required by design sections 7.9 and 10.2. Bind source/checker manifest, input A64IR, target, live ranges, assignments, spills/frame slots, call clobbers, SP facts, output, pass/schema versions, and payload digest. The producer and validator are separate packages; GPU witnesses remain with GPU stage owners. Acceptance: the Wave 2 allocation validates, while mutations to any binding, range, overlap, register, spill slot, call effect, SP fact, or digest reject before layout/encoding or promotion.

Resolution 2026-07-31: subsumed by the landed allocator pair. src/compiler/native/regalloc.f publishes claims and src/compiler/native/regalloc-verify.f is a separate package that re-derives every live interval from the frozen module and rejects wrong bindings (E-A64RAV-MODULE, -CONTRACT), wrong ranges (-INTERVAL), missing or extra coverage (-COVER), out-of-contract registers (-REGISTER), overlaps (-OVERLAP), class errors (-CLASS), broken ties (-TIE), and stale acceptances (generation counter, -STATE) - the mutation matrix in test/compiler/native-regalloc.f kills all of them before anything can emit. What this dot added beyond that was a serialized certificate with a payload digest, and that has no consumer: the whole chain runs in one process over a frozen module, where identity implies content. Spill slots and frame facts belong to habu-lower-spills-and-ef14a0dd, fixed-register and calling-convention facts to habu-bind-arm64-arg-f76afa3a, and if a portable witness ever gains a real consumer (cross-process emission), that is a new dot then.
