---
title: Remove tier 0 copies for dup, over and back-edge renames
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T20:21:25.393911+02:00"
closed-at: "2026-10-27T22:36:39.107918+02:00"
close-reason: "Landed sharing (two tag-0 VS entries share a register, split on write) and slot-preferring pulls. Bench stacked on tailframe: share zz 13.032G tender 277.22G; + pull zz 12.731G tender 277.11G (base zz 13.799G, tender 282.33G)."
---

zz per-byte loops spend register copies tier 1 does not: PARSE-FILE-CONTENT 3 copies per byte (dup, over, back-edge rename; 21 vs 17 insns), SCAN-CHAR 2 (16 vs 13), SCAN-SPACE 3; zz moves 0.87G of 1.86G. Tender FIND-DOCUMENT 2 copies per miss. Acceptance: dup/over of a register value consumed before either copy is written emits no mov, and a back-edge reconcile whose layout differs only by a renaming the loop body can absorb emits no copies; tier0-inline-prims counts the movs. Design: shared registers (two tag-0 VS entries with one register, split on write via three-operand destinations) and slot-preferring pulls in LVNEED. Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: jerry, workspaces jerry-share and jerry-pull.

Pull lane review (jerry-pull): values identical on 62 probe words; the slot guess is not free. NEED-ALLOC maps final VS index f to frame slot f from the bottom while LVRECONAT matches from the top, so a back edge whose VS is shallower than the frame, or a `swap` between pull and back edge, turns an exact base back edge into copies (Q12 `1+ swap 1+ begin swap CAL swap over + dup 2000 > until +`: +3 movs per iteration). Probe tally 23 movs saved, 9 added. Lane bench alone: zz 13.799G -> 13.657G, tender neutral; it lands only if it still gains stacked on the sharing lane.
