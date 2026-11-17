---
title: Keep Wasm float arithmetic and element addresses inline
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T14:42:41.283567+01:00"
---

Problem: in Maki's viewer module (Wasm, built by Habu), a run of a 1000-place view (a 10x10x10 rack, Maki dot maki-find-what-a-80491fa2, hunt vwalkcost 2026-11-17) spends 53-56% of the module's own time in functions outside viewer.f, and four Habu routines take 30-33% of it: functions #25 and #21, read as f+ and f*, are out-of-line calls that pass their operands over the memory stack and canonicalize NaN (10%); #85 and #105, read as a buffer element's address with a bounds check (from their callers POSE@, WALK-M/CLIP-DOT and SPAN@ and their trapping bodies), 20-23%. Observed: Chromium CPU profiles sampled at 100 us, function numbers matched to a marker build (/tmp/tim-vwalkcost/prof, names.txt, bin/). Inferred, not traced: the routines' identities. Acceptance: in a Wasm module, f+ f- f* f/ compile inline on Wasm values with no call and no memory-stack round trip where the compiler can hold them in locals, and an element address's bounds check is inline or hoisted out of a loop it cannot fail in, keeping each word's meaning (NaN results and traps as now); measured on the same view's profile, those routines' share gone. Depends: none. Claim: unassigned.
