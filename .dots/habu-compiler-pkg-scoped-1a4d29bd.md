---
title: "Compiler: package-scoped model definer"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T03:28:16.756886+02:00\""
---

cad-1 follow-up (worker finding): MODEL: capture runs body tokens through checked planning words, but the body is not compiled into a single checker-verified word - fully static composition checking needs a package-scoped colon-style defining word: open the planning package, compile the body as a real definition over tensor descriptors, seed inputs from the signature, so the checker verifies the whole composition statically (CAD-PLAN section 3 full vision) AND named value references become possible (current linear-consumption capture cannot express DAG references like residual to an earlier intermediate). Compiler work authorized. Depends: cad-1 (landed). Related: habu-maki-subsystem-pkgs (package machinery), TFAM sealed-package rules.
