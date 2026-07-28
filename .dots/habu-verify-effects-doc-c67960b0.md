---
title: Verify effects doc R8 substrate section
status: open
priority: 3
issue-type: task
created-at: "2026-07-28T15:42:48.655640+02:00"
---

Full context: the effects-grammar verification swept docs/effects.md lines 1-780 with a fixture and exit code per production. The R8 / CAD-EFFECT half (roughly lines 780-1200, about 40 percent of the file) was read but not tested. Most of it is explicitly forward-looking with owning dots, and two cited files (maki/effect-bindings.f, maki/effect-projection.f) do not exist yet - but src/cad/effect-types.f and src/cad/effect.f DO exist and are described in present tense, so the R8-0 substrate section is testable today and was not tested. Two smaller unverified claims from the same sweep: the linear-kind section claims KEEP/BI/TRI are not sound linear capability boundaries - measured only for a concrete linear (rejects, exit 70), the polymorphic laundering case was not constructed, so the claim is neither confirmed nor refuted; and E-UNCHECKABLE vs the old E-UNCHECKED verdict spelling was found in render.f:558 but the structured-diagnostic emitter was not exercised. Acceptance: every present-tense claim in the R8-0 substrate section verified by loading a fixture; the polymorphic KEEP case constructed and the document corrected or confirmed; the verdict spelling exercised through the structured emitter; same method as the main sweep - exit codes captured directly, document states only what is measured.
