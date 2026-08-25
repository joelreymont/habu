---
title: Model field projection window
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:04:24.778566+02:00"
---

Full context: one of the omission leaves split out of habu-model-the-declared-4a2eb3c9. The parent dot named construct and field projection as one omission; the code shows two different rules, so they are split. Construct is habu-model-construct-row-bcdd5ef6. This leaf is field projection.

What is being discharged. formal/Common/Effects.v names "the generated-accessor introduction window" alongside the transport ops and, separately, "construct (CONM) and field projection"; formal/Common/Control.v names "the field projection window". This leaf removes the field-projection clause from both headers and, if it also models LAYOUT-INTRO, the generated-accessor clause; otherwise it leaves that to habu-model-whole-bundle-9589e059.

What the checker actually decides today with no model behind it. field-project is a reserved checker operation, not a word. FIELD-PROJ! (src/core/checker.f:9130) arms a single-shot window with the accessor name, a committed field id and a baked byte offset; FIELD-PROJ-MATCH? (checker.f:9133) fires only when the word currently under check IS that armed accessor; FIELD-PROJ-STEP (checker.f:9143) then peeks a pointer to a layout family below a baked offset literal, asks FIELD-PROJ-XT for the instantiated field type, and produces a pointer to that field type through the ordinary step so linearity and row plumbing stay consistent. Every validation failure calls FIELD-PROJ-REJECT and fails closed. The hook's other half, TFAM-FIELD-PROJ (src/core/type-family.f:2528), refuses a field the input family does not own, a baked offset that disagrees with the committed offset, a non-addressable field role, an arity mismatch, and a field extent past the family width. This is the single shape the ordinary layout fence refuses, so nothing else in the language can retype a layout pointer.

Where the rule belongs. formal/Common/Effects.v, beside the layout machinery, because the whole rule is row surgery plus a validation predicate on a committed field record. The results worth stating: the projection fires only for the armed accessor and only once; a baked offset that disagrees with the committed offset is refused; and the produced type is the field's schema instantiated over the INPUT pointer's family arguments, so a generic family projects at its instantiation and not at its declaration.

The vector shape that would bind it, and the one real obstacle. The window is armed only by the generative crossing, so no ordinary candidate can reach it: test/field-proj-suite.f arms it through a named TRUSTED forwarder (FP-ARM, test/field-proj-suite.f:43) and that is the only way a test gets in. The parity gate's cases file would need the same forwarder plus a PRODUCT declaration and committed field ids, which adds a named unchecked boundary to a file that currently has none. Decide that deliberately before writing code: either accept the forwarder with a comment saying why the sealed window cannot be reached otherwise, or bind the rule structurally instead of behaviourally. Record the decision in this dot.

The mutation that must go red. Drop the committed-offset equality from TFAM-FIELD-PROJ-DO (src/core/type-family.f:2521) so a forged offset is accepted; the forged-offset row must flip from refused to certified. Restore the source byte-identically and record the matrix.

Blocked by nothing, but read habu-model-construct-row-bcdd5ef6's landed vectors first: it establishes how the gate asks the checker a question that only a particular scope can answer, and the same device may apply here.
