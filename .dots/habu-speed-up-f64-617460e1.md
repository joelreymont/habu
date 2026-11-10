---
title: "Speed up F64-TEXT:PARSE on texts with a far exponent"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T23:22:19.428145+02:00"
---

Problem: src/lib/f64-text.f PARSE is exact multi-limb arithmetic; on a text whose exponent is far from zero it is slower than the libc PARSE it replaced. Measured on macOS arm64, 600000 calls of one text per run, whole process: 2.2250738585072012e-308 takes 5.34 s against 0.63 s with libc; 6.644, -1.5e-7 and 123456789.125 take 0.46, 0.40 and 0.52 s against 0.54, 0.55 and 0.56 s, so short texts with a near exponent are not slower. The cause inside PARSE is not measured: the number for that text is about 36 limbs, and the shifts and the divisions by ten are suspected. Probes: ~/Work/reviews/habu-10x-2026-11-10/briefs9/f64-probe/timing-1.f to timing-4.f. Acceptance: first measure where the time goes for that text (which words, how many limb operations); then PARSE on it costs no more than twice libc's time by the same measure, with answers still identical to libc on the plist.f listing in the same directory. Files: src/lib/f64-text.f, src/lib/f64-text-test.f. Verify: the four timing probes before and after; bin/hb --load src/lib/f64-text-test.f; the plist.f comparison. Depends: habu-format-a-binary64-477e6a35 landed. Ownership: unassigned. Claim: unassigned. Consumers: Maki PARSE (10 calls), Tender PARSE (2); no consumer reported it.
