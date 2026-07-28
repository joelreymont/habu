---
title: Store compiler operations
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:58.866674+02:00"
blocks:
  - habu-define-compiler-dialect-28e79b1c
---

Full context: design section 6.3 requires append-only operation, value, operand, result, and successor pools. Add builder-only append/read APIs with owner IDs, typed windows, source spans, schema references, and overflow checks; no freeze yet. Acceptance: bounds/window/arity/foreign-owner/overflow fixtures pass and no frozen/public mutation API exists. Dependency: dialect schemas.

Note 2026-07-28 (arena landing): IR-CTX's per-context mapping is a 64K
creation-time constant (MAP-BYTES in src/compiler/ir/context.f) and IR-ARENA
sizes ceilings as creation parameters. Real function/op tables must pick named
ceiling constants here and may need the context mapping enlarged or chunked;
that sizing decision belongs to this table layer, not to the arena.
