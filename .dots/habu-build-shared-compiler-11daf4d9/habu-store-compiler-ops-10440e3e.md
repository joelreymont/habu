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
