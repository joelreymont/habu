---
title: Model T-ATOM rigid host identities
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:02:43.415137+02:00"
---

Full context: this is one of the omission leaves split out of habu-model-the-declared-4a2eb3c9, which asked for one leaf per declared omission rather than one big change.

What is being discharged. The header of formal/Common/Effects.v lists, under "Deliberate omissions from the modelled fragment", the line "T-ATOM rigid host identities (region / extent / generation)". Remove exactly that line when this leaf lands, and leave the other omission lines alone.

What the checker actually decides today with no model behind it. src/core/checker.f mints an atom term with MK-ATOM / MK-ATOM-K (checker.f:376-383) and stamps it with a rigid host-allocation identity drawn from one of three private per-check counters: RGN-FRESH, EXT-FRESH and GEN-FRESH (checker.f:358-367), routed by the name prefix of a template fresh atom in RIGID-AK-MINT (checker.f:370-374). RIGID-RESET (checker.f:348) restarts all four counters at 1 for every check, and RIGID-MAX (checker.f:333) is deliberately below the sign wrap so a domain throws E-RIGID-EXHAUST rather than wrapping and handing out an identity that is still live. Two atoms unify only through ATOM-OK? (checker.f:1272), which qualifies the numeric id by its domain kind, so equal numbers from different domains never unify. None of that is in either model file.

Where the rule belongs. formal/Common/Effects.v, in the type-term section next to the con/ty vocabulary: an atom is a new ty shape carrying a domain and an id, ATOM-OK? becomes a decidable equality on the pair, and unification's atom arm calls it. The two facts worth stating as results are that identities from two different domains never unify however their numbers compare, and that fresh identities are never reused within one check (monotonicity of the counters), which is what makes an allocation's region, extent and generation meaningful at all.

The vector shape that would bind it. Two shared program vectors in test/compiler/checker-model-schema.f whose verdicts differ in class: a definition whose signature names the same rigid atom twice and certifies, against one that names two atoms from different domains and is refused. Derive any number that appears (a domain code, a counter start) structurally from the checker's own constants rather than writing it down twice, exactly as the CMV15-CMV18 rows derive the match depth guard from the frame ceiling.

The mutation that must go red. Make ATOM-OK? (checker.f:1272) ignore the domain and compare the raw ids: the cross-domain row must flip from refused to certified and the gate must fail on exactly that row. Restore src/core/checker.f byte-identically afterwards and record the matrix in the dot.

Blocked by nothing. It touches the type vocabulary only, so it does not wait on the construct, transport or match leaves.
