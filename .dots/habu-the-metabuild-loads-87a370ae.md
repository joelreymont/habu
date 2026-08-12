---
title: The metabuild loads the core prefix twice
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T09:29:59.556348+02:00"
---

Found by the prewindow lane (2026-08-12, proven three ways: layout-buffer-seal's probe prints twice in one hb-stdin-mk process at DP 5114599 then 9321220; a base-prefix word is E-UNDEFINED by assembled-source time; window name resolution reaches the SECOND copy): hide.f truncates the dictionary back to SEQ (util.f:13, the first prefix file), so the assembled source recompiles the entire 4.2MB core prefix on top of the orphaned first copy without rewinding DP - a ~0.4s and 4.2MB tax on every metabuild, and the structural cause of the pre-window non-isomorphism that killed the carry design. Investigate whether the truncate point can sit after the core prefix (keeping the boot copy live and the layouts order-isomorphic) without violating what hide.f's truncation protects - that would speed every metabuild AND make host/target prefix layouts identical, simplifying every future bake question. Feeds the boot-must-not-recompile reframe on e98b03d4. Files: src/habu/hide.f, src/habu/stdin.f. Depends: none.
