---
title: Remove positional buffer duplicates
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T23:30:04.428672+02:00"
---

The new positional buffer layer duplicates existing loaded primitives. maki/embedding.f ROW-ADD is byte-for-byte semantics of maki/array.f T-ADD! with arguments reversed; WPE-SLICE is exactly maki/move.f MOVE-SLICE with source shape maxt x dim and rows [0,t), but adds another loop, WPE-FIT and E-WPE-EXTENT; WPE-SLICE-ADD only computes t*dim and calls ROW-ADD. Public TOKPOS-EMBED is EMB-GATHER followed by that duplicate add and has no non-test caller. All four public/private helpers, WPE-FIT and the error exist only for embedding tests, while the actual MODEL composition uses GATHER ADD and does not consume them. With identical maki/array.f preload, the positional production change grows embedding.f from 5 to 11 definitions and from 1,828 to 3,216 JIT bytes: exactly +6 definitions, +1,388 JIT bytes and zero DATA. Delete ROW-ADD, WPE-FIT, E-WPE-EXTENT and the test-only wrappers. Express buffer goldens through existing MOVE-SLICE and T-ADD!, or through the real batched model operation owned by habu-complete-batched-pos-99332bf6; do not preserve aliases. Prove exact forward/VJP goldens through canonical primitives, old names reject, no production consumer is lost, current valid generated model behavior is unchanged, and the measured six definitions/1,388 JIT bytes are removed or every retained byte is attributed to a real consumer. Files: maki/embedding.f/tests. Packaging remains habu-own-embedding-pkg-9e22b2b0; bounds remain habu-validate-embedding-access-2cf51e2c.
