---
title: Wrap the lines over 80 columns in fmath and the browser fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T11:41:02.454234+02:00"
---

Problem: docs/forth-card.md:139 says lines are 80 columns at most; these files on master 443d62cc break it: src/lib/fmath.f:6, 116, 134, 135, 138, 140, 309; src/lib/fmath-test.f:135, 137, 140, 227, 241, 243, 244, 254, 256, 257, 261, 349, 351, 440-442, 461; test/browser/navigation.f:85, 117, 128; test/browser/navigation-test.f:75, 108, 111; test/browser/display.f:42, 43, 48, 68 (found while adding FMATH:ACOS and the view's turn records; their lanes keep their own new lines within 80). Acceptance: awk 'length>80' over these five files prints nothing and their tests pass. Depends: none. Claim: unassigned.
