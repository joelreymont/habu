---
title: Fix wide PRODUCT minimum-input accounting
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:11:22.234037+02:00"
---

Full context: finite CAD effect redesign requires a defensible multi-binding row. Native checked probes show a PRODUCT of physical width 34 fails while generating ROW:UNMAKE with checker: min-in exceeds record field rc76, and a width-12 PRODUCT certifies alone but minimal PAIR ( row row -- row row ) fails identically; width 11 is the largest repeated row tested. The true input minima are 34 and 24, far below ER.MINI u8 limit 255, so src/core/checker.f ROW-CELLS or copied effect-row resolution cycles/overcounts repeated wide nominal products. Fix root cause in width traversal/effect recording; do not raise the field limit or shrink consumers. Acceptance: generated MAKE/UNMAKE for W34 records exact minima; identity/PAIR over two W34 rows records 68 and certifies; same-family repetition, nested wide product, quotation, stored-word replay, rollback, snapshot, native fixpoint and bootstrap parity pass; an actual physical minimum above 255 rejects deterministically with the named diagnostic and no registry mutation. Add red-first minimal fixtures and assert stored ER.MINI values. Files: src/core/checker.f, focused wide PRODUCT/effect-record suites, bootstrap mirror only if the native model duplicates this logic, docs/effects.md if semantics change. Verify: focused checker/type suites, typed-local diff lint, bootstrap/fixpoint, host/filemap/dot lints, full native gate. Unblocks habu-define-finite-cad-0bdf52ad; ownership is checker physical-width accounting only, not CAD effect design.
