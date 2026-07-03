---
title: "EPIC: type habu ground-up, retire TRUSTED"
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:46:48.554565+02:00"
---

Goal: eliminate TRUSTED:/TRUST as a category. TCB today: EMIT-HOST-LOAD-PREFIX loads util/structures/checker/render with HOOK-CELL=0 (src/habu/habu2.f:412-415), 226 TRUSTED: defs + ~307 TRUST rows repo-wide. Strategy: (1) staged fixpoint checking - stage N binary CHECKS stage N+1 source (incl checker.f, render.f, builder habu1/habu2/jit) before building it, so nothing in SOURCE is unchecked (habu-self-check-checker-e10ce327 is the first rung); (2) convert TRUST rows on builder emit words to real CHECKED: definitions - reg/label/asm roles already exist for this; (3) discharge each TRUSTED: via a new checker capability (depth introspection typing, records, linear resources) or rewrite as checked code; (4) shrink the irreducible axioms to a single audited primitive-effect table with per-primitive differential tests. Irreducible remainder: primitive axioms + the seed binary (Thompson trust) - document as the explicit trust root. Track per-class progress via habu-trusted-inventory dot.
