---
title: "TFAM 11b: open-arg linear-bundle lift"
status: active
priority: 2
issue-type: task
created-at: "2026-07-17T22:43:19.456689+02:00"
---

The dedicated feature TFAM 11's closure record specifies (habu-tfam-11-linear-99fa9990, see its SOUND LIFT SCOPE section - that analysis IS the spec): lift the over-conservative open-arg-bundle transport reject as one coherent slice: (a) expand open-arg bundles to fixed width while preserving construct's raw->hidden coercion for partially-open bundles (rework SUNI/LOGHID so a hidden field carrying an open var arg unifies with a resolved hidden field); (b) extend deferred linear taint to layout arg vars (XG-TAINT-SEQ/LIN-TAINT taints copied scalar var groups only - a copied open-arg bundle must taint its arg vars so a later linear binding rejects, mirroring LIN-TAINT-SCAN); (c) verify call-site LIN-CHECK + LIN-EFF-PASS/EN-PARAM catch linear instantiations of generic bundle transports; (d) lift the MATCH open-arg reject (E-MATCH-OPEN-ARGS/MB19) once the scrutinee expands - note the MATCH consume/refine-exactly-once leg also waits on TFAM item 9 by design. Pins that FLIP when it lands (the acceptance): TDLIN-VAR-DUP/TDLIN-VAR-TOR (type-decl-suite), MB19/B19 (type-match-suite), ZP8/P5/P6 - flip each to a positive with the negative preserved in adversarial form. No minimal subset exists (the dot's item 4: a 1-cell move of a W-cell value miscompiles). Engine prefix work: full battery (fixpoint x2, old-binary boot, run.f perf verdict, seal/pin/PEINV, strict inventory). Files: src/core/checker.f, type-family fixtures, the pin suites. Ownership: checker capability (TFAM program).

Claim: agent=tfam_11b_impl workspace=.jj-ws/habu-tfam-11b-open-ee9c72c6 machine=spark
