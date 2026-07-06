---
title: Checker self-typing
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.902871+02:00"
---

The final rung: typed rewrite of util.f(27)+structures.f(41)+render.f(451) then checker.f(~3939 ln) so BF-CERTIFY-STAGE certifies the FULL stage source and flips to blocking - after which the trusted base = seed + 192 PRIM axioms + ~153 builder-emit rows + ~59 named class-c boundaries. Pre-checker-style obstacles: no signatures, raw create arrays (TVT/RVT/PTRA), tag-packed cells (TAG/PAY/MK-CON) - aided by ptr-arith, typed-defining-words, dict-record capabilities landing first. Effort L (1wk+, re-split after BF-CERTIFY-STAGE reports the real miss list - the miss list IS the work plan). Closes habu-epic-type-habu to its irreducible core.

## Scope refresh (2026-07-06)

src/core/render.f now certifies clean as the tail of its exact boot-prefix
context (typed cleanup under habu-make-fixpoint-certify-a11dbad5;
BFT-TEST-CERTIFY-TFAM-PREFIX pins it with the real file, stub retired), and
the whole generated stage2/stdin sources certify rc 0 with BLOCKING
enforcement in the fixpoint. Remaining scope here: checker.f's own 7 TRUSTED:
rows and the staged-prefix bake-check (previous-stage binary certifying
checker.f/render.f before baking, per habu-self-check-checker-e10ce327).
