---
title: "Add the constructor's pads at a wide instantiation"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T00:49:31.003456+02:00"
---

Found by the width-export landing (37fad2bd): a CONSTRUCTOR call at a parametric instantiation needing extra pads does not add them - lib/object.f NEXT-LINE's none arm constructs OPTION:NONE at option<obj:line> and refuses E-NELAB-JOIN (-8503), fail-closed, no silent miscompile (measured). The checker already publishes the number (TFC-CON-XPAD-RECORD) - the same shape of work as the landed MATCH half, one door over: latch it in the REC unit against the token ordinal, read it at the ctor call, pad the construction. Last E-NELAB-MATCH-family row in the census. Files: src/core/checker.f (latch+export), src/compiler/native/{dict,elaborate}.f. Depends: none (the MATCH landing is the worked example).
