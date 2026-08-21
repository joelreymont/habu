---
title: Build native branch SSA
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:59:19.971850+02:00\""
closed-at: "2026-08-14T11:51:17.241079+02:00"
close-reason: "Closed SATISFIED (Wave-3 audit 2026-08-14): IF/ELSE/THEN/EXIT as explicit blocks with successor arguments (elaborate.f 4854-6510), one return block by construction, joins validate width/glue/park to E-NELAB-JOIN with the registered negative inventory (native-elaborate.f 1690-1800), no hidden snapshots (block args are the only carrier), differentials live-reproved through NMIGRATE:DEFINE. Note: the leaf says SIR; the shipped tree is HIR to A64IR, no SIR layer ever existed. Residue owned by 7e013b93 (whose own description has aged - see its leaf note)."
---

Full context: design Wave 3 adds IF/ELSE/THEN and EXIT as explicit SIR blocks, successor arguments, one exit block, and no hidden stack snapshot. Acceptance: joins validate exact typed arguments; missing/extra/wrong-type successor values and multiple implicit exits reject; differential zero/nonzero and nested branches pass.
