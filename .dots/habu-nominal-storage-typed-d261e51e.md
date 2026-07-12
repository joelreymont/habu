---
title: "Nominal storage: typed definers"
status: open
priority: 2
issue-type: task
blocks:
  - habu-nominal-storage-effect-d5ced9ba
created-at: "2026-07-12T15:48:44.761062+02:00"
---

Phase 3 of habu-checker-seal-nominal-0b2eaece after effect parametricity. Generalize LAYOUT-BUFFER validation into CHECKER-STORAGE-INFO for closed non-linear addressable arity-zero TYPEFAMILY, nominal atom, closed layout family, and closed typed pointer. Add uppercase TYPED-VARIABLE and TYPED-BUFFER with checked extent, overflow, zeroing, typed accessor effects, transactional allocation/definition rollback, persistence and fixpoint/bootstrap parity. Reject open variables, quotations, linear values, hidden fields, unresolved args, duplicate names, capacity failure. No TYPED-CREATE. Add checked-producer TYPED-CONSTANT only if producer effect is proven.
