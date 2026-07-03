---
title: Primitive-effect axiom table + difftests
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.831796+02:00"
---

Consolidate scattered primitive effect assertions (PRIM: rows in src/core/checker.f:2167-2216 PES table + TRUST rows on engine primitives) into ONE audited axiom table - the explicit, minimal trust root for typing. Each axiom gets a differential test: execute the primitive on generated stacks and compare observed depth/value behavior against the declared effect (extend test/prop-test-core.f machinery once habu-unfreeze-checker-prop lands). Deliverable: docs section naming the axiom set + gate suite proving every axiom has a difftest; inventory ratchet counts axioms separately from discharged TRUSTED.
