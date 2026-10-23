---
title: Move internal-word-gate JSON cases
status: open
priority: 3
issue-type: task
created-at: "2026-10-23T17:33:29.550372+03:00"
---

Problem: internal-word-gate checks its two JSON cases in separate tools/check.f children (~2.6 s CPU); requiring tools/check-core.f in the gate's parent pollutes its forked subjects (3 cases fail), and its 15 '99 PKG:tail !' store legs are redundant (both legs die at the name lookup).
Acceptance: the two JSON cases run in test/diag-position-test.f, which already loads the CHECK core; the redundant store legs are deleted; every distinct assertion is kept.
Verification: both suites pass on their own with wall/CPU before and after.
