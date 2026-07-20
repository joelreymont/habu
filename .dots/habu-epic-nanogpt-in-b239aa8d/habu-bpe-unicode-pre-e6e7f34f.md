---
title: "BPE: unicode pre-split closure"
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T23:03:34.681237+02:00"
blocks:
  - habu-bpe-real-vocab-c973932a
---

Loose end from the real-vocab BPE landing (808d6c99): the pre-split matcher is exact for pure-ASCII runs AND pure-non-ASCII runs, but diverges from the GPT-2 regex exactly when a multi-byte letter/number codepoint abuts an ASCII letter/digit (pinned divergence fixture: naive -> engine [2616,26884,303] vs tiktoken [2616,38776]). Close the class: a UTF-8 codepoint decoder + unicode Letter/Number category membership over the codepoints the vocab can produce (a generated bounded table with provenance, not a full UCD import), extending the matcher's letter/digit classes so the divergence fixture flips to MATCH. Red-first: the divergence fixture asserts MATCH after, and the old both-sided pin fails. Measure the table's DATA cost.
