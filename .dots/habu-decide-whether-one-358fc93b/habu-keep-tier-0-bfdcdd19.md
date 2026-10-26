---
title: Keep tier 0 stack values in registers
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:18.221570+02:00"
closed-at: "2026-10-26T14:31:01.277735+02:00"
close-reason: step 2 landed and measured
---

Experiment step 2. Pop physical-stack operands into pool registers on demand (C-VBIN-TAGS jit.f:313-318 returns mode 0 when VSP<2; EM-COMPILE-CALL spills before calls habu2.f ~12229-12260) and add VUN/VOP rows for @ c@ cells cell+ ptr-field (! keeps PROT-GUARD:CALL). zz category b = 40% of the tier gap. Measure zz, tender, kernels, tools/compile-floor.f.

Design (Codex Astra standing in for Fable, 2026-10-26; text in ~/.cache/tmp/jerry-step2-design.md): one helper LVNEED pulls up to two physical cells into pool registers beneath the virtual suffix (C-VBIN-TAGS, VBINI probe, FBINPREP, VUN-ENTRY, VSHUF-ENTRY); call spills, LVSNAP/LVRECON and locals unchanged. Part 2 adds rows for @ c@ cells cell+ ptr-field. Lane part 1: jerry, worker in .jj-ws.noindex/jerry-step2; handoff ~/.cache/tmp/jerry-step2-handoff.md. Lane part 2: jerry, worker in .jj-ws.noindex/jerry-step2mem (stacked on part 1); handoff ~/.cache/tmp/jerry-step2mem-handoff.md. Claim: jerry.

Part 1 landed bfc5830d, part 2 (memory rows @ c@ cells cell+ ptr-field) landed in "Keep tier 0 memory operands in registers"; both full suites 499/499, two-gen byte-identical. Remaining: the design's section 4 measurement (engine at 9d3c13ce vs the part 2 engine; tier-bench, compile-floor, zz daily, tender c2-office-scale, dots ls/tree), then close.
Measurement lane: jerry, worker over .jj-ws.noindex/jerry-s2base (9d3c13ce) and jerry-s2cand (2ff923b9); results ~/.cache/tmp/jerry-s2measure/; handoff ~/.cache/tmp/jerry-s2measure-handoff.md.

Result (engine 9d3c13ce vs 2ff923b9, user CPU medians, run = whole - load; host heavily loaded, per-rep ratios steady; outputs identical across base t0, cand t0, cand t1): run cost at tier 0 fell 37% tier-bench (.62 -> .39 s; t1 .33), 35% zz daily over 43,723 JSONL lines (2.55 -> 1.65; t1 .87), 18% dots ls x300 (2.23 -> 1.82; t1 1.48), 20% dots tree x300 (4.60 -> 3.68; t1 3.32), 27% tender c2-office-scale (75.8 -> 55.5; t1 39.2). That closes 54-79% of the t0->t1 run gap; tier 0 run is still 1.11-1.90x tier 1, so the 5% retirement goal is not met. Compile cost unchanged (compile-floor tier 0 trivial 43 us on both). DOC:FIND-DOCUMENT x19 loads/stores 13/11 -> 8/5, DOC:F-NEXT 18/17 -> 6/5.
