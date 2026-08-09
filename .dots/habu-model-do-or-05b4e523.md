---
title: Model do, or name the missing opener
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:54:02.171194+02:00"
---

The dialect has ?do and loop but no do; 4 definitions refused. WORSE SINCE THE FOLD (merged ce6488f7): LOOP now folds, so CROSS-SCAN's LS-POP closes a loop nothing opened and throws E-NELAB-CTRL BEFORE the walk reaches DO - the 4 are still refused (never miscompiled; probed) but no longer named, which is why DO left the census histogram: re-labelled, not fixed. Either model do (the counted loop whose limit pair is already on the stack - same machinery as ?do minus the zero-trip guard) or make the refusal name the missing opener at the do site. Acceptance: the 4 definitions either compile-and-agree with the engine or refuse NAMING do; a census run shows them attributed correctly. Files: src/compiler/native/{hir-word,elaborate}.f. Depends: none.
