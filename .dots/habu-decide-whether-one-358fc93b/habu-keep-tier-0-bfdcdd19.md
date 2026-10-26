---
title: Keep tier 0 stack values in registers
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:18.221570+02:00"
---

Experiment step 2. Pop physical-stack operands into pool registers on demand (C-VBIN-TAGS jit.f:313-318 returns mode 0 when VSP<2; EM-COMPILE-CALL spills before calls habu2.f ~12229-12260) and add VUN/VOP rows for @ c@ cells cell+ ptr-field (! keeps PROT-GUARD:CALL). zz category b = 40% of the tier gap. Measure zz, tender, kernels, tools/compile-floor.f.

Design (Codex Astra standing in for Fable, 2026-10-26; text in ~/.cache/tmp/jerry-step2-design.md): one helper LVNEED pulls up to two physical cells into pool registers beneath the virtual suffix (C-VBIN-TAGS, VBINI probe, FBINPREP, VUN-ENTRY, VSHUF-ENTRY); call spills, LVSNAP/LVRECON and locals unchanged. Part 2 adds rows for @ c@ cells cell+ ptr-field. Lane part 1: jerry, worker in .jj-ws.noindex/jerry-step2; handoff ~/.cache/tmp/jerry-step2-handoff.md. Lane part 2: jerry, worker in .jj-ws.noindex/jerry-step2mem (stacked on part 1); handoff ~/.cache/tmp/jerry-step2mem-handoff.md. Claim: jerry.
