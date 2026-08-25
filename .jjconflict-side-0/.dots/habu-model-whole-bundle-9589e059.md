---
title: Model whole-bundle transport ops
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:03:18.872047+02:00"
---

Full context: one of the omission leaves split out of habu-model-the-declared-4a2eb3c9.

What is being discharged. Both model headers declare it. formal/Common/Effects.v lists "the whole-bundle TRANSPORT ops (XPORT-STEP?, checker.f:7215) and the generated-accessor introduction window", and explains that each arms one of two checker registers, LAYOUT-XPORT and LAYOUT-INTRO (checker.f:1465-1466), and that those two registers are the ONLY thing that opens LAYOUT-BLOCK?; the file deliberately models the ordinary window in which both are clear. formal/Common/Control.v names the transport ops again in its own omission list. Remove the transport clause from both headers when this leaf lands, and leave the generated-accessor window to habu-model-field-projection unless this leaf models it too.

What the checker actually decides today with no model behind it. LAYOUT-XPORT-TOK? (src/core/checker.f:9051) recognises the token spellings that move a layout value as one bundle, and DO-TOK1 arms LAYOUT-XPORT from it for exactly one token (checker.f:9168). While it is armed, WF-XPORT-RECORD (checker.f:7049) records the width facts from the pre-op row and XPORT-STEP? (checker.f:7277) performs the row surgery directly instead of running the ordinary word effect. The register also opens LAYOUT-XPORT-ALLOW? (checker.f:1573), which is the single exception LAYOUT-BLOCK? (checker.f:1598) grants: outside a transport op a layout cell may not bind a variable at all. A possibly-linear layout is still refused even inside the window (checker.f:1456-1461). Local capture arms the same register for the same reason (checker.f:7530).

Where the rule belongs. formal/Common/Effects.v: the two registers become fields of the substitution-or-state the unifier reads, layout_blockb gains the allow arm, and the transport step becomes an explicit row surgery beside push_logical and layout_push_fields. formal/Common/Control.v needs the token that arms the register for one step, because arming is a property of the token machine and not of unification. The results worth stating: a layout cell binds a variable only while the transport register is armed; the arming lasts exactly one token; and a possibly-linear layout is refused even armed.

The vector shape that would bind it. A verdict-class-changing pair of shared program vectors: a whole-bundle move of a layout value through a transport token, which certifies, against the same move written with an ordinary word of the same declared effect, which is refused because the layout fence closes. A second pair should put a linear inside the bundle so the armed-but-still-refused case is bound too.

The mutation that must go red. Make LAYOUT-XPORT-ALLOW? (checker.f:1573) return true unconditionally instead of requiring the armed register; the ordinary-word row must flip from refused to certified. Restore src/core/checker.f byte-identically and record the matrix.

Depends on nothing already landed, but it shares the layout machinery with habu-model-construct-row-surgery; do not start both in one workspace.
