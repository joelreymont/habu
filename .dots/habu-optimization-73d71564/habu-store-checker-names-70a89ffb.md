---
title: Store checker names as interned offsets
status: open
priority: 1
issue-type: task
created-at: "2026-09-24T17:18:43.056587+02:00"
---

Checker symbols contain 85,022 duplicate package-name bytes. Both string fields are absolute pointers into one 251,306-byte arena, creating 33,378 relocation rows costing 267,024 bytes. A measured arena-relative offset encoding costs 97,765 value bytes before changed bitmap/code/alignment. Symbol keys are already unique; this concerns string storage and pointer representation, not duplicate symbol IDs.

Intern package spellings and represent symbol strings with arena-relative offsets, following the existing CT/VREC offset convention. Preserve stable symbol IDs, case folding, global/private/public identity, empty package spelling, arena growth/rebase, lookup and diagnostics. Never substitute borrowed token-buffer pointers. Keep any transient acceleration index out of captured state.

Own src/core/checker.f SYM-PKG!, SYM-COPY-FOLD, string accessors, snapshot persistence and pointer marking. Verify lookup across collisions, visibility, growth, rollback, capture/restore and subsequent definitions using existing behavior suites; demonstrate reduced string and address-row sections on rebuilt hb. Run native fixpoint, test/run.f and Maki smoke. Can proceed independently of private-symbol pruning, but serialize edits to the checker persistence code. Representation and pruning savings overlap.
