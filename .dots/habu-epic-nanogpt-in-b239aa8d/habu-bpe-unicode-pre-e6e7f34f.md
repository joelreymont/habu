---
title: "BPE: unicode pre-split closure"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-20T23:03:34.681237+02:00\""
blocks:
  - habu-bpe-unicode-integration-d3e95a72
---

This is the Unicode pre-split umbrella. The landed matcher and complete
`White_Space` table are useful foundations, but the bounded Letter/Number table
is not a valid domain: GPT-2's base byte vocabulary can encode arbitrary
Unicode. Exact children own pinned Unicode data generation, strict reentrant
UTF-8 scalar decoding, and BPE integration/parity. The umbrella closes only
after all three prove complete classification and exact reference behavior.

Claim: RELEASED 2026-07-21. The `unicode` workspace is preserved as evidence,
but its bounded-block Letter/Number implementation is obsolete and must never
merge. The already-landed decoder and White_Space foundation may be retained
only after the complete, pinned, checked proof above validates them.
