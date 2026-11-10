---
title: font-native test fails when build/ is absent
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T14:31:35.974733+02:00"
closed-at: "2026-11-10T14:43:40.802427+02:00"
close-reason: "landed; the test wrote three SVG files nothing read, so the writes are deleted; bin/hb --load test/font-native.f prints test: ok, rc 0, in a workspace with no build/"
---

Problem: test/font-native.f writes build/font-*.svg and fails E-FS-OPEN (-2102) in a tree with no build/ directory (seen in lane jerry-o1, a fresh workspace). Acceptance: the test passes in a fresh workspace; it writes under TMPDIR or makes its directory. Files: test/font-native.f. Verify: bin/hb --load test/font-native.f in a workspace without build/. Depends: none. Ownership: test/font-native.f. Claim: unassigned.
