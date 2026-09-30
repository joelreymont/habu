---
title: Profile and speed up the engine self-build
status: closed
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.724031+03:00"
closed-at: "2026-09-30T17:30:00.000000+02:00"
close-reason: "Dropped: the profile it asks for is docs/compiler-measurements.md sections 3 to 8, and the 30-second target has no design behind it. A native build takes 87 s against the 138 s recorded here."
---

Problem: tools/native-build.f compiles the 6.2 MB guard-free engine in 138 s on this machine (5354 certified words in the window, about 26 ms per word; the guarded pin took 95 min). A Forth compiler should build itself in seconds; nothing in the per-word work justifies tens of milliseconds. No build profile exists yet: candidates are the write-xor-execute mprotect flips per emitted word, checker registry scans, the AOT capture passes and the fixpoint self-check. Acceptance: a perf profile of one native-build run attributed to engine words (tools/imgdump.f --pc), the top cost fixed at its root, before/after timings in LESSONS.md, the build under 30 s with a byte fixpoint preserved. Files: tools/native-build.f, src/habu/habu2.f, src/habu/aot-capture.f, src/habu/aot-lib.f. Verify: time HOST --load tools/native-build.f -- OUT from a quiet tree snapshot, twice, cmp the two outputs. Depends: none (related: habu-compile-the-prefix-609867f1 measures the optimizing tier at 60 ms per word). Ownership: hazel line. Claim: unassigned.
