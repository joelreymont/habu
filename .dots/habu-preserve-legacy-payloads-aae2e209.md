---
title: Preserve legacy payloads in full ENUM
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T17:07:10.057551+03:00"
---

Retiring SUMTYPE requires full ENUM to accept its existing payload schemas. Astra baseline on c316 reports box<n> and quotation fields refuse7109, box<ptr n> accepted by legacy SUMTYPE but not STRUCTURE’s partial application parser, and direct value recursion exits102/stack bounds instead of refusing. Share existing complete schema parsing and owner-aware width operations without changing family kinds, generated names, field order, C2 validity or pointer-recursion support. Scope is coupled to habu-convert-the-type-d5aad352. Write the migrated declaration-header E2E and recursion rejection before code. Acceptance includes accepted generic/quotation/multi-token pointer payloads and bounded direct-value-recursion refusal through real load/verify paths; full native/convergence and Astra review.
