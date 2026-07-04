---
title: "Compiler: package-scoped model definer"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T03:28:16.756886+02:00\""
---

cad-1 follow-up (worker finding): MODEL: capture runs body tokens through checked planning words, but the body is not compiled into a single checker-verified word - fully static composition checking needs a package-scoped colon-style defining word: open the planning package, compile the body as a real definition over tensor descriptors, seed inputs from the signature, so the checker verifies the whole composition statically (CAD-PLAN section 3 full vision) AND named value references become possible (current linear-consumption capture cannot express DAG references like residual to an earlier intermediate). Compiler work authorized. Depends: cad-1 (landed). Related: habu-maki-subsystem-pkgs (package machinery), TFAM sealed-package rules.

UPDATE 2026-07-04: the named-value half LANDED on fable (maki/cad.f capture
layer, no src/core needed): signature input names bind, >V NAME names the
current value, a bare NAME pushes a parameter operand (FIFO queue; positional
capture provably unchanged). True skip connections + fan-out DAGs work; FFN-SKIP
in maki/demo-ffn-test.f proves node.3.in "n2 i0" and an OP-ADD summed x-gradient;
fail-closed paths in maki/cad-ref-test.f. REMAINING (this dot stays open): the
full CAD-PLAN section 3 static composition - compile the MODEL body as ONE
checker-verified definition over tensor descriptors (package-scoped colon-style
definer), so the checker proves the whole composition statically instead of the
capture layer resolving refs at parse time. v1 limitation to lift with it: a
named ref supplies PARAMETER operands only; the data operand is always the
running value (H1 GELU cannot re-root the chain).
