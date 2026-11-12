---
title: Wrap the lines over 60 columns in fmath and the browser fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T11:41:02.454234+02:00"
---

Problem: docs/forth-card.md:139 says lines are about 60 columns at most (Joel 2026-11-12: "the limit is not 80 columns but around 60 i would say, for human readability"); src/lib/fmath.f, src/lib/fmath-test.f, test/browser/navigation.f, test/browser/navigation-test.f and test/browser/display.f have lines over it. A line that is one string literal handed whole to a word (TREE at navigation.f:85, 117, 128; T-LABEL in navigation-test.f; SVG in display.f) cannot be wrapped: Habu has no way to write a literal across lines. Acceptance: awk 'length>60' over these five files prints nothing and src/lib/fmath-test.f and test/wasm/device.f pass. Bookmark carl/wrap-fmath (on a4d24d48) holds a reviewed wrap of fmath and the fixtures to 80 columns; it conflicts with master in src/lib/fmath.f and test/browser/navigation-test.f. Depends: habu-write-a-long-91f11bb3, for the literal lines only; the release (Joel 2026-11-12: 'we can delay this until after release'). Claim: unassigned.
