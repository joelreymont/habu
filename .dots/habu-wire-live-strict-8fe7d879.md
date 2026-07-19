---
title: Wire live STRICT-OWNERS into the trust gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T17:44:34.803903+02:00"
---

Review finding 13 (pin 8195257e): the strict trusted-inventory run reports 1053 trust sites and exits 81 (one invalid owner) but tools/trusted-inventory-test.f:82 only checks COUNTS, never runs live STRICT-OWNERS - so the gate trust slice is green while production strict is red. Fix: run the production strict command inside the gate slice (fail-closed), after the invalid row lands via habu-remove-drain-pretrust-509d64a0 (SERIALIZE behind it, else master goes red on wiring day). Then start the 680-TRUSTED:-definition reduction under its own dots.
