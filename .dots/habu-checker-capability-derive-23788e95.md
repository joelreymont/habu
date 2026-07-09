---
title: "Checker capability: derive eq/order/hash for ADTs"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:43:23.706621+02:00"
---

Convenience gap from type-family review: docs/type-families.md:1763 lists automatic deriving of equality/order/hash as a v1 non-goal, so every maki ADT hand-writes comparisons. After TFAM 15/16 land, add opt-in derived words (e.g. DERIVE eq order hash inside SUMTYPE/PRODUCT blocks) generated from family/variant metadata like constructors: checked effects, no trust rows, exhaustive over variants, hidden fields never exposed. Reserved-token + replay + public-signature treatment identical to generated constructors (PLAN.md items 5, 8, 13 patterns). Depends: TFAM 8, 15, 16.
