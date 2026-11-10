---
title: No suite runs the font tests
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:13:21.959918+02:00"
---

Problem: test/font-native.f and test/font-image-subject.f are in no SUITE of test/gate-*-cases.f, so src/lib/font.f has no test the suite runs. font-image-subject.f's header describes a hook that is gone ('live FONT handles are retired before hb-build writes hb'), it writes build/font-image.svg (fails where build/ is absent), and its RUN checks the stale handles before it opens another font, so it would not catch a serial that restarts (src/lib/font.f SERIAL, fixed by the zip/font/genio counter dot). Acceptance: a suite 'font' on macOS runs font-native.f and a build-level case: font-image-subject.f is built by tools/hb-build.f and its program opens another font and image first, then is refused E-HANDLE for the two kept at top level; it writes no file. Files: test/font-image-subject.f, test/gate-stdlib-cases.f, a driver beside them. Verify: bin/hb --load test/run.f -- font. Depends: the counter fix. Ownership: unassigned. Claim: unassigned.
