---
title: Multishot quotations + typed DOES>
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.884593+02:00"
---

Two capabilities: (1) multi-shot/recursive quotation types for combinators - BI/TRI/TIMES/EACH/MAP/FOLD (src/core/combinators x6), TILE-LOOP/ACC-LOOP (~8 sites; tile-loop.f claims existing machinery may suffice - verify first); (2) typed DOES>/defining-word effects (structures-effects x6) and fold roles.f 34 nominal casts into DEFTYPE auto-derivation (also serves habu-declarable-nominal-int-3b0721cc). Effort M (~4d).

PREMISE CORRECTED 2026-08-10 (quotations design lane): multishot quotations
bound to a local and executed inside a ?do loop already CHECK today
(lib/array.f A-MAPI!/A-FOLDI/A-SCAN!/A-FIND-INDEX, lib/sort.f HS-CHILD/
HS-STEP). What keeps src/core/combinators.f unchecked is ROW VARIABLES
(( R a [ R -- S ] -- S a )) plus return-stack-held quotations (r@ execute) -
not multishot. Re-scope this leaf to those two; it does not gate the chain's
quotation stages (04341c80/1ea5f813/a1e4a21e).
