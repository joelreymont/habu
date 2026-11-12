---
title: Wrap the lines over 60 columns in word-docs.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T11:52:30.575909+02:00"
---

Problem: docs/forth-card.md:139 says lines are about 60 columns at most (Joel 2026-11-12: "the limit is not 80 columns but around 60 i would say, for human readability"); src/lib/word-docs.f has lines over it (found while moving its vectors). Acceptance: awk 'length>60' src/lib/word-docs.f prints nothing and src/lib/word-docs-test.f passes. Depends: habu-move-vec-s-4edbc913 (it rewrites the file's vector lines; wrap after it lands). Claim: unassigned.
