---
title: "Maki: cad.f missing transitive lib/float require"
status: done
priority: 2
issue-type: task
created-at: "2026-07-05T00:43:20.138396+02:00"
---

Ablation-lane finding: MODEL: capture sets the float default tolerance via POW10 but maki/cad.f does not require lib/float.f transitively - masked in the gate by load order; a standalone consumer needed an explicit require lib/float.f. Fix: add the require to maki/cad.f (or wherever POW10 is consumed), remove workaround requires in consumers, prove by loading a minimal cad.f consumer standalone.

RESOLVED 2026-07-06. Root cause was deeper than cad.f: the missing requires were
in the POW10-consuming *libraries* themselves, masked everywhere by gate load
order. Fixes: (1) lib/float.f consumes STR-DIGITS?/STR-DIGIT-VALUE/STR-MINUS/
STR-PLUS from lib/string.f but never required it -> added `require lib/string.f`;
(2) lib/fmt.f consumes POW10 (float) and the SB builder (string) but required
neither -> added `require lib/float.f` + `require lib/string.f`; (3) maki/cad.f
explicitly `require lib/float.f` since its public GOLDEN word drives the
float-tolerance golden compare (was only satisfied incidentally via golden.f).
The ablate-fusion-test.f workaround `require lib/float.f` (the only such site;
its comment mis-attributed the dependency to MODEL: capture) was removed.
Proof: `bin/hb --load lib/float.f`, `... lib/fmt.f`, and a standalone
`require maki/cad.f` + `MODEL:`/`GOLDEN` consumer all load green with no explicit
float require; each bare load previously died E-UNDEFINED (STR-DIGITS? / POW10).
