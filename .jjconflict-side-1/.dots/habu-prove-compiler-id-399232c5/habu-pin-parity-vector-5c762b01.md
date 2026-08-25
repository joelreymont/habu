---
title: Pin parity vector row counts
status: open
priority: 3
issue-type: task
created-at: "2026-07-28T13:23:17.885863+02:00"
---

Full context: the compiler identity parity gate catches a weakened vector row, because the schema digest changes and both sides are then asked the new question. It does not catch vector-row SHRINKAGE where the digest is deliberately re-frozen to match: deleting rows and re-recording the digest is caught only by human review. test/compiler/ir-id-manifest.f requires every vector-reachable guard to be hit at least once, so coverage cannot reach zero, but it could fall to one row per guard while still passing. Required result: pin the row counts of the three vector tables (pack, check, scalar) in the manifest so a deletion fails even when the digest is re-frozen. Acceptance: deleting one row and re-freezing the digest fails the gate; adding a row is a deliberate count change; the gate stays green unmutated. Small.
