---
title: Store checker names as interned offsets
status: closed
priority: 1
issue-type: task
created-at: "2026-09-24T17:18:43.056587+02:00"
closed-at: "2026-09-24T21:27:41.073354+02:00"
close-reason: Intern package spellings and store arena-relative string offsets while preserving the pointer-view owner ABI. Independent review, native byte fixpoint, 490/490 native suites and Maki routing/export pass. Isolated saving 280704 bytes; integrated engine 2972407 bytes. Evidence ~/.cache/tmp/habu-opt-names-scratch/RESULTS.md.
---

Checker symbols contain 85,022 duplicate package-name bytes. Both string fields are absolute pointers into one 251,306-byte arena, creating 33,378 relocation rows costing 267,024 bytes. A measured arena-relative offset encoding costs 97,765 value bytes before changed bitmap/code/alignment. Symbol keys are already unique; this concerns string storage and pointer representation, not duplicate symbol IDs.

Intern package spellings and represent symbol strings with arena-relative offsets, following the existing CT/VREC offset convention. Preserve stable symbol IDs, case folding, global/private/public identity, empty package spelling, arena growth/rebase, lookup and diagnostics. Never substitute borrowed token-buffer pointers. Keep any transient acceleration index out of captured state.

Own src/core/checker.f SYM-PKG!, SYM-COPY-FOLD, string accessors, snapshot persistence and pointer marking. Verify lookup across collisions, visibility, growth, rollback, capture/restore and subsequent definitions using existing behavior suites; demonstrate reduced string and address-row sections on rebuilt hb. Run native fixpoint, test/run.f and Maki smoke. Can proceed independently of private-symbol pruning, but serialize edits to the checker persistence code. Representation and pruning savings overlap.

Design reviewed before implementation: retain the five-cell symbol record with arena-relative string offsets; intern package spellings through process-local HIDX bucket/next tables, maintained with symbol append, retirement, rollback, growth and rebuild. Preserve the pointer-based SOURCE-ROW owner ABI through a transient decoded row and clear it before capture.

Failure modes to check: offset zero mistaken for absence; empty/global or private/public identity collapsed; folded spelling and package-hash collisions; borrowed token spans or existing offsets invalidated by growth; stale package chains after rollback or explicit source retirement; SOURCE-ROW decoded with the wrong owner's arena; transient transfer pointers or symbol offset cells captured as address rows; restored compilation failing; native generations not converging. Use existing engine, scan-index, rollback, package/import, owner-handover and capture E2Es, then native generations and physical section measurements. Integrator owns the combined full gate and Maki smoke.

Implementation, independent review and integrated acceptance complete. Two successive standalone native products from frozen sources are byte-identical at 3,104,503 bytes, down 280,704 from the 3,385,207-byte baseline. Address rows shrink from 270,552 to 3,528 bytes; the string arena's captured owner cost shrinks from 286,764 to 189,806 bytes. Engine, scan-index, signature rollback, using, package scope/replay, AOT chain capture and native-window-owner suites all pass. Worker evidence: `~/.cache/tmp/habu-opt-names-scratch/names-results.md`. The integrated engine is 2,972,407 bytes, reaches a native byte fixpoint, passes all 490 suites and preserves Maki's routed/exported PCB bytes; its existing negotiation suite also passes.
