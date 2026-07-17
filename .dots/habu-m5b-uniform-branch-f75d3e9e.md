---
title: "M5b: uniform-branch acceptance + explicit barrier marking"
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T14:45:50.288591+02:00"
---

Remainder of habu-ptx-m5-mask-eb0716f1 (2026-07-17, commit e87cb494): the landed model soundly REJECTS any block collective inside an open control frame, which over-rejects the legitimate case of a collective under a provably block-uniform branch. Work: (1) per-CF-frame uniformity tracking - when an if/begin/do condition is uniform<bool> (already a distinct family), mark that frame uniform; accept a CTL-BARRIER call when ALL enclosing frames are uniform; negatives: lane-varying condition still rejects, mixed nesting (uniform if inside varying if) still rejects; (2) explicit barrier marking for words that emit bar.sync internally WITHOUT the tile->uniform shape: BLOCK-MAX-SELECT ( uniform tile uniform -- tile ) emits a block-min internally and is invisible to the structural detector - add an explicit per-word barrier declaration surface (directive or effect annotation) and mark it; negative: BLOCK-MAX-SELECT inside an if rejects. Same engineering rules as M5: engine prefix work (checker.f), fixpoint x2, old-binary boot, full battery + run.f perf verdict, negatives wired into the gate, never accept your own negative fixtures. The E-ADD-EFFECT choke + PTX-BARRIER-SET-XT hook pattern from M5 is the template (see LESSONS 2026-07-17). Files: src/core/checker.f, lib/ptx/uniform-barrier-test.f, docs/type-families.md 9.1.2 update. Ownership: checker capability.
