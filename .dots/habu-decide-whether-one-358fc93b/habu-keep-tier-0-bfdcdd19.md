---
title: Keep tier 0 stack values in registers
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:18.221570+02:00"
---

Experiment step 2. Pop physical-stack operands into pool registers on demand (C-VBIN-TAGS jit.f:313-318 returns mode 0 when VSP<2; EM-COMPILE-CALL spills before calls habu2.f ~12229-12260) and add VUN/VOP rows for @ c@ cells cell+ ptr-field (! keeps PROT-GUARD:CALL). zz category b = 40% of the tier gap. Measure zz, tender, kernels, tools/compile-floor.f.
