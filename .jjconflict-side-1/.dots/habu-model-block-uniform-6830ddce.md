---
title: Model block-uniform branches
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:04:42.973346+02:00"
---

Full context: one of the omission leaves split out of habu-model-the-declared-4a2eb3c9.

What is being discharged. The header of formal/Common/Control.v lists "uniform<bool> block-uniform branches (COND-UNIFORM?, checker.f:7778-7799) and the block-collective barrier rule (ALL-CF-UNIFORM?, checker.f:7681)", and adds that Effects.v now models the arity-1 families these need, so what is still missing is the CF.UNI frame slot, the PTX-TILE-FAM / PTX-UNIFORM-FAM recognition and the barrier control flag - none of it representational. Remove exactly that clause when this leaf lands.

What the checker actually decides today with no model behind it. COND-UNIFORM? (src/core/checker.f:7841) asks whether the top of the data row resolves to the registered uniform family (PTX-UNIFORM-FAM, checker.f:486). CF-IF consumes that flag and marks the frame it opens as block-uniform by setting CF.UNI (checker.f:7861); CF-PUSH clears the slot by default (checker.f:7719), so a quotation or match frame is never uniform. ALL-CF-UNIFORM? (checker.f:7745) then answers whether EVERY open frame is uniform, and DO-TOK1 refuses a block-collective token with REJECT-DIVBAR unless it is (checker.f:9192, via BARRIER-CUR? at checker.f:6430). The rule exists because a block-collective barrier inside a divergent branch is unsound: some lanes reach it and some do not.

Where the rule belongs. formal/Common/Control.v. The frame record gains the CF.UNI field, cf_push writes it false, do_if reads a uniform-family flag off the condition type and writes it, and the barrier token is a new tok whose rule is the all-frames-uniform test. The two registered family ids ride in the configuration the same way the type-family registry already does. The results worth stating: a barrier inside any non-uniform frame is refused; a barrier inside only uniform frames is accepted; and a quotation or match frame is never uniform, so wrapping a barrier in a quotation cannot launder it.

The vector shape that would bind it. A verdict-class-changing pair of shared program vectors: a barrier inside a branch on a uniform<bool>, which certifies, against the same barrier inside a branch on an ordinary bool, which is refused. A third row should nest the accepted case inside a quotation and stay refused. The uniform family and the tile family have to be registered for the checker side to recognise them, so this leaf must first establish how the gate's cases file registers them; record that before writing vectors.

The mutation that must go red. Make ALL-CF-UNIFORM? (checker.f:7745) answer true unconditionally; the ordinary-bool row must flip from refused to certified. Restore src/core/checker.f byte-identically and record the matrix.

Blocked by nothing.
