---
title: Bound remaining maki/db element accessors (CONE@ DEP@ BUDGET-AT@)
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T22:51:44.357176+02:00"
---

Follow-up to habu-bounds-check-action-39819fc1 (its audit table, 2026-07-20): maki/db/diagnostic.f CONE@ (~line 856) and maki/db/obligation.f DEP@ (~line 700) are public accessors refining an unchecked element index k into a nominal artifact-id via CONE-AT/OB-DEP-AT - the same raw-index-then-refine defect ENUM-AT had; maki/db/transaction.f BUDGET-AT@ (~line 880) plus the diagnostic string-list readers (EXPECTED@/OBSERVED@/INVALIDATED@/REPAIR@ via unguarded SL-GET) take unguarded k but return scalars/spans (no nominal mint - lesser severity). Blocked from the actbounds lane because a correct guard needs a new per-package error code and the diagnostic (-5354..-5358) and obligation (-5359..-5365) blocks are contiguous-full: this dot owns the small error-registry reallocation (or an adjacent free block) plus the guards, canary/property tests per the ENUM-AT pattern, and coordination with the open diagnostic/obligation dots.
