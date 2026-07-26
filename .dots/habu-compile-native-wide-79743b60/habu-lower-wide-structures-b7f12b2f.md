---
title: Lower wide structures
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:39.028178+02:00"
blocks:
  - habu-lower-native-compile-ca9e5541
---

Full context: design Wave 6 under the final declaration syntax lowers `NEWTYPE` and `STRUCTURE` layouts from one HIR build into explicit representation witnesses. Acceptance: field order/alignment/multi-cell construction, locals, copies, target policy, and witness mutations validate; source is not reparsed.
