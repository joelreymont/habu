---
title: Wrap the lines over 80 columns in word-docs.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T11:52:30.575909+02:00"
---

Problem: docs/forth-card.md:139 says lines are 80 columns at most; src/lib/word-docs.f breaks it at lines 21, 36, 39, 41, 43, 45, 108, 285 on master a4d24d48 (found while moving its vectors; that lane keeps its own lines within 80). Acceptance: awk 'length>80' src/lib/word-docs.f prints nothing and src/lib/word-docs-test.f passes. Depends: habu-move-vec-s-4edbc913 (it rewrites the file's vector lines; wrap after it lands). Claim: unassigned.
