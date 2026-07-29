---
title: Model MATCH scrutinee pop
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T00:05:03.636384+02:00\""
---

Full context: one of the omission leaves split out of habu-model-the-declared-4a2eb3c9.

What is being discharged. The header of formal/Common/Control.v lists "MATCH's SCRUTINEE POP, which stays abstracted here", and is precise about the gap: the checker walks a width-expanded hidden-field bundle cell by cell (MATCH-SCRUT?, checker.f:8236-8246) while the model's match_scrut requires one arity-0 logical TFam cell. It also records that Effects.v now has everything that pop needs - t_width, fam_hid and layout_push_fields - so this is undone work rather than a missing representation, and that every rule ABOVE the pop (payload refinement, the branch join, MD-JOIN, exhaustiveness) is modelled exactly. Remove exactly that clause when this leaf lands.

What the checker actually decides today with no model behind it. MATCH-SCRUT-CELL? (src/core/checker.f:8284) checks one bundle level top-down and MATCH-SCRUT? (checker.f:8294) walks the whole width-expanded bundle off the row, answering the row below it; MATCH-SCRUT-DIAG (checker.f:8304) classifies a failed scrutinee. The model's match_scrut (formal/Common/Control.v, in the MATCH section) short-circuits all of that to a single-cell test, so today a match on a real multi-cell layout value is outside the modelled fragment even though every rule around it is inside.

Where the rule belongs. formal/Common/Control.v, replacing match_scrut in place with a walk written over Effects.v's t_width, fam_hid and layout_push_fields. Nothing above it should need to change; if it does, that is a finding worth writing down. The results worth stating: the pop consumes exactly the family's instantiated width and leaves the row below untouched; a bundle of the wrong family is refused whatever its width; and a partially popped bundle is never left on the row.

The vector shape that would bind it. A verdict-class-changing pair of shared program vectors over a sum family with a real multi-cell payload: a match that pops the whole bundle and certifies, against one whose scrutinee is a different family of the SAME width, which must be refused. Same-width is the sharp case, because a width-only test would pass it. This leaf also unlocks the stronger statement the depth lane wanted, that a match at the deepest frame that fits actually CERTIFIES rather than merely being unresolvable; add that row if it comes out of the work.

The mutation that must go red. Make MATCH-SCRUT-CELL? (checker.f:8284) ignore the family id and check only the cell count; the same-width different-family row must flip from refused to certified. Restore src/core/checker.f byte-identically and record the matrix.

Blocked by nothing, but the certifying deep-match row also wants habu-model-construct-row-bcdd5ef6 landed, because construct is how a body mints a family value of a known variant.

Claim: agent=scrutinee workspace=.jj-ws/habu-model-match-scrutinee-8d897108
