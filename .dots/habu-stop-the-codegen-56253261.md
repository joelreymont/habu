---
title: Stop the codegen-compare entry calibration from flaking
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T16:52:14.945387+02:00"
---

tools/codegen-compare.f reports 'ENTRY the two empty calls no longer measure the same call: old ~3.4 ns, new ~2.0 ns' on one run in two, on an idle Apple-silicon host, and it is not a compiler change: measured on the 2026-08-04 tree and on its parent with the same binary, the two calibration rows land on different core classes and the ratio moves past the band. It costs nothing today because the SCHEDULED member is tools/codegen-compare-test.f, which runs with COSTS-UNCHECKED, but the by-hand timed entry is the one a lane is told to read before and after a change that is meant to move the numbers, and a finding that appears without a cause trains people to ignore it. Behavior: make the entry's empty-call calibration measure both columns under the same conditions - interleave the two calibration runs, or take the median of several, or pin the comparison to a ratio the file's own band already tolerates - and state the measurement the fix rests on. Files: tools/codegen-compare-calibrate.f, tools/codegen-compare-core.f, tools/codegen-compare-baseline.f. Verify: ten consecutive bin/hb --load tools/codegen-compare.f runs on an idle host, 0 finding(s) each. Depends: none. Found by the float-fuse lane (habu-fuse-a-float-4545c786) while running the timed entry as a gate.
