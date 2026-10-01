---
title: Build live ranges without a per-value block walk
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:32:11.943545+02:00\""
closed-at: "2026-10-01T17:39:58.972447+02:00"
close-reason: Fixed by psnsluvx 09be83fa (review 174 ACCEPT)
---

Problem: src/compiler/native/regalloc.f:1119 MB-EXTEND-V walks every block for every value, so MB-RANGES costs values x blocks. After r4-nest commit 3 (refit) it is the largest allocation cost: at 64 conds the fit takes 66 ms, MB-RANGES 976 ms, MB-LIVENESS 212 ms, planning 289 ms (least of 3; ~/.cache/tmp/kestrel-r4-nest/c3). Acceptance: ranges built from each block's live-in and live-out sets; every function allocates exactly as before (masked per-word dump compare over the loop and frame suites, engine g1 == g2, two-generation build); MB-RANGES time at 8, 16, 32, 48, 64 conds near-linear, measured in user CPU; native-wide-frame row CPU before and after. Files: src/compiler/native/regalloc.f. Verify: native-regalloc, x64-regalloc, native-wide-frame and the nest suites.
