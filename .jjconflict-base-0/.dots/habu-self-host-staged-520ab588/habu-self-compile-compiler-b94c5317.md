---
title: Self-compile compiler modules
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:00.501900+02:00"
blocks:
  - habu-retire-native-byte-f44351f7
---

Full context: design Wave 8 and bootstrap theorem require every new checked compiler module to compile through the staged compiler itself. Acceptance: module-by-module candidate loads pass with exact stage digests, no legacy fallback, no new trust boundary, and complete source/checker/environment bindings.
