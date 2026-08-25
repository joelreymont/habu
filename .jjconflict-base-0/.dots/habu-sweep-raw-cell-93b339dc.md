---
title: Sweep raw-cell storage of nominal values
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T15:18:46.599604+02:00"
---

Full context: four sites (LEW-INS in maki/lower/ew.f, LRED-INS in red.f, LMM-INS in mm.f, LMV-INS in move.f) stored a MIR:operand-ref nominal into a raw create-cells-allot array with plain store. The checker rejects that — expected a-ptr-a, actual MIR:operand-ref-ptr-a — and it reproduces in six lines with a NEWTYPE, a create array and a cells-plus-store body. All four were found only by accident, when the cast-ownership fix removed the throw that masked them, and all four were repaired there by converting to LAYOUT-BUFFER typed storage. Sweep the rest of the tree for the same pattern rather than waiting for the next unmasking: every create-name-cells-allot whose accessor signature names a family or DEFTYPE nominal. Prefer a checked lint over a manual search so it stays enforced, and schedule it in both test/gate-stdlib-cases.f and test/gate-stdlib-inline-lib.f. Acceptance: the lint runs clean over the tree, is scheduled, and any site it finds is converted to typed storage rather than cast around.
