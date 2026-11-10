---
title: "C4: a build's declarations write the product's records"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:30:41.274095+02:00"
---

Problem: inside a build, the product's declaration words run as product code; each must write the facts of the product's own records through CHECK:FACTS! (src/core/checker.f:13484), which holds them for the linker while a build's space is open, rather than into tables hb's checker never reads. TRUST (checker.f:23294) and INTRINSIC (:18498, through RECORD-CONTROL-AT!) now do, by HOLD-AMENDED (:13524) and src/core/check-hook.f AMENDED (:318-324). Still open: TYPED-VARIABLE (src/core/layout-buffer.f:476) arms LAYOUT-INTRO (checker.f:3453, set at :27492, read at :3685, :3702, :3907) to admit its generated accessor instead of writing that record's facts; `names:` (src/core/cell-effects.f:84) is answered through TOP-BINDING (checker.f:23843), and STRUCTURE's family registers through SD-REGISTER (src/core/structure-decl.f:327), neither checked yet against this rule. The census that listed the writers is gone; restate it from the tree. The product's created words keep record bytes 32-39 (their facts offset) at 0 until habu-c5-the-linker-01e00e9d, since nothing lays their created text before the linker's prelude; hb's per-build stores carry them.
Acceptance: every declaration writer finds its record through the dictionary and writes the product's record's facts with CHECK:FACTS!; LAYOUT-INTRO is replaced; the list of writers checked is in the commit.
Files: src/core/checker.f, src/core/layout-buffer.f, src/core/structure-decl.f, src/core/cell-effects.f, src/core/type-family.f, the definers.
Verify: `rg -n LAYOUT-INTRO src` prints nothing; the linker test; bin/hb --load test/run.f.
Depends: none.
Ownership: unassigned. Claim: unassigned.
