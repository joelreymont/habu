---
title: Align CALL-EDGE-MASK over an expanded logical input
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.768350+02:00"
---

Problem: CALL-EDGE-MASK (src/core/checker.f) maps a callee's keep bits by live term position; a callee whose declared input is one logical cell over a live group the caller expanded to W terms (or the reverse) misaligns the bits. Found by reading (stuart, hbind fix, 2026-11-07); conservative in the first direction, untested in both. Verify first: a caller passing an expanded option<pr> to a callee declaring option<a>, and the reverse, checked for the keep mask. Acceptance: the mask follows cells, not terms; a test that fails before. Files: src/core/checker.f, a test under test/. Verify: that test. Depends: none. Ownership: carl. Claim: unassigned.
