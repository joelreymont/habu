---
title: Validate fetched ADT representations
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:43:48.968038+02:00"
---

Critical soundness: src/habu/habu2.f EMIT-P2-FETCH promotes arbitrary cells to typed sums/enums/products without representation validation. Establish typed-storage provenance or validate tags/nested representations before certification; add invalid-tag, padding, and product-field regressions. Dependency: pointer provenance design.
