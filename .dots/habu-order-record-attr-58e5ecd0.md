---
title: Order record attr keys by symbol bytes
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T21:50:50.357021+02:00"
---

Full context: IR-ATTR:RECORD canonically sorts its pairs by symbol ORDINAL, which makes attribute identity independent of pair presentation order within one module — but symbol ordinals are insertion-ordered, so that is not the design section 6.3 line 479 canonical order. The section 6.6 canonical encoder must emit record keys in symbol-BYTE order, as it already must for the symbol and type tables. Acceptance: two modules that intern the same symbols in different orders encode byte-identical records. Dependency: the canonical encoder.
