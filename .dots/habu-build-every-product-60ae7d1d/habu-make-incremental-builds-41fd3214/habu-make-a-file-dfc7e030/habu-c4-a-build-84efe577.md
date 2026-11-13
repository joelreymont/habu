---
title: "C4: a build's declarations write the product's records"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:30:41.274095+02:00"
---

Problem: inside a build, the product's declaration words run as product code; each must write the facts of the product's own records through CHECK:FACTS! (src/core/checker.f:12147), which holds them for the linker while a build's space is open, rather than into tables hb's checker never reads. TRUST (checker.f:21412) and INTRINSIC (:16958, through RECORD-CONTROL-AT! :16715) do, by HOLD-AMENDED (:12704) and src/core/check-hook.f AMENDED (:267). Still open: TYPED-VARIABLE (src/core/layout-buffer.f:441) arms LAYOUT-INTRO (checker.f:4009, set at :24331, read at :4167, :4192, :4319) to admit its generated accessor instead of writing that record's facts; `names:` (src/core/cell-effects.f:84) is answered through TOP-BINDING (checker.f:21864), and STRUCTURE's family registers through SD-REGISTER (src/core/structure-decl.f:407), neither checked yet against this rule. The list of writers is to be restated from the tree. The product's created words keep record bytes 32-39 (their facts offset) at 0 until habu-c5-the-linker-01e00e9d, since nothing lays their created text before the linker's prelude; hb's per-build stores carry them.
Acceptance: every declaration writer finds its record through the dictionary and writes the product's record's facts with CHECK:FACTS!; LAYOUT-INTRO is replaced (`rg -n LAYOUT-INTRO src` prints nothing); the list of writers checked is in the commit.
Depends: none.
Claim: unassigned.
