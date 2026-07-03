---
title: "TFAM 15: product families + value-record/FIELD migration"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.958843+02:00"
---

PLAN.md item 15. FIRST migrate pre-existing FIELD words (lib/object.f:97, lib/object-test.f:28, src/habu/aot-lib.f:211 + call sites) before reserving PRODUCT/FIELD/END-PRODUCT (case-folded). Implement products after layout-aware ops proven; decide by evidence whether VALUE-RECORD becomes sugar or typed compat layer; migrate PTX IR only after by-value fixtures + no size regression. Gate 17o. Depends: TFAM 12 (order: after 14).
