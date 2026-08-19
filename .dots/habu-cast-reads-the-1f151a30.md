---
title: "CAST: reads the next definition off the stream"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T15:52:11.871693+02:00"
---

USER-RULED FINAL (2026-08-19, supersedes both earlier designs on this leaf's
history): `CAST: NAME ( from -- to )` manufactures a TYPE SIGNATURE and nothing
else. The checker resolves the two declared signature terms in the current
scope, runs the five CAST-CERTIFY structural refusals on them (7129 arity, 7130
class, 7131 family, 7135 owner, 7137 linear - properties of the terms, never of
a body), and publishes NAME carrying that signature with identity behavior. No
definition is manufactured anywhere: no generated source text, no evaluate
crossing, no CAST-PEND window, no second spelling.

SUPERSEDED, with the evidence that killed each: (1) the original sketch
"CAST: delegates to : on the live stream" - impossible: `:` is a reader
construct, not an executable word (stepper.f:63-76), and no input-cursor
restore exists before layout.f loads, so lookahead is destructive at roles.f's
row. (2) CAST-NEXT: (a second stream spelling) - rejected as ceremony: it kept
the definition ritual and added a form; the user ruling is one CAST:, zero
definitions.

WHAT THE BUILD DELETES (the payoff, to be listed in the landing report): the
text generator in roles.f, CAST-EVAL, the CAST-PEND window machinery in
checker.f, the generated-text fidelity caveat, and the measured drift hazard
(a name-keyed window with no positional bound certifies against a definition
arriving arbitrarily later) - which ceases to exist rather than being repaired.

OPEN QUESTIONS the probe answers before building: (a) the checker's signature
resolver (the path that turns a definition's stack comment into SGIN/SGOUT rows
during CHECK, proven live at roles.f's row 894 by the boot probe) - what does
it need to run OUTSIDE a definition, as a contained checker.f change;
(b) the body census over the ~100 live CAST: sites - production casts are
expected empty-body; a cast with code is not a cast (it is a checked word plus
a cast, written as two things), and test-fixture shuffles belong to phase 7
(dc125344); (c) publication - reuse the axiom-path spine (name+effect without a
checked body) with certify in front, identity xt from the engine;
(d) whether existing sites' trailing `;` becomes unnecessary and the mechanical
sweep count if so.

First consumer: roles.f's 34 TRUSTED: casts - no load-order problem remains
because there is no evaluate left to arm. Then the rest of 1f5980b8's sweep.
