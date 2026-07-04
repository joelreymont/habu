---
title: "Checker: linear values launder through typed locals"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T09:51:04.456613+02:00"
---

Live soundness hole, probe-proven on bin/hb at b4a95381: 'deflinear own  : BAD ( own -- own own ) {: x:own :} x x ;' CERTIFIES (tools/check.f exit 0) while the equivalent 'dup' correctly rejects (exit 70, E-REJECTED). Local references bypass LIN-CHECK count conservation: binding a linear into a {: :} local then referencing it twice duplicates it; an unreferenced linear local is silently dropped (also uncaught). Found by the TFAM-11 census (docs/census-tfam-11.md C1/R4, includes the LIN-CHECK/CHECKER-STEP/LOC-REF? site map). Fix in src/core/checker.f: local binding of a linear-counting type must either consume-and-track (each reference re-counted, unreferenced = leak reject) or be rejected outright; add negative regressions for double-reference, zero-reference, and reference-after-consuming-call; positive for exactly-one reference. Blocks the item-11 layout-linear guarantees (habu-tfam-11-linear-99fa9990); independent of layout values — scalar linears affected today.
