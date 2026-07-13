---
title: "gaps: dead + test-only surface cleanup (dead-code audit)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T10:41:51.440245+02:00\""
---

From the dead-code audit (2026-07-13, goal e93371de). DELETE (true dead, zero refs): NODE#/NODE@ (maki/onnx/import.f:590,592; also E-ONNX-IDX if orphaned), SPACE-LOCAL (maki/tensor.f:97). REMOVE minted-ahead-without-owner (one-liners, trivially re-added with consumers): COLS+/COLS-/DIM-IS?/SHAPE-IS? (tensor.f; ROWS siblings live, enum-symmetry does not satisfy no-dead-code). SHRINK trusted boundary: SPACE-RAW (tensor.f:34, TRUSTED) + ADDRESS-SPACE-EQUAL?/DECODE/VALID?+SPACE-GLOBAL/SHARED+RANGE reachable only from tests - either remove the cluster + TRUSTED.md row, or keep ONLY with a documented declared-ahead owner comment (device address-space work is zed-gated: if kept, comment must name the pending-zed consumer + the dot). INLINE: SK-TARGET$ (sched-key.f:237, test-only wrapper) - migrate sched-key-test.f:87 to SB-RESET SK-TARGET+ SB$ and delete. DECLARED-AHEAD (keep + comment, do NOT delete): TARGET:DIGEST@/EQUAL?/FACTS$/LABEL$/RESOLVE/COUNT + DESC-HASH chain - V2 sec 9.1 content-addressed object identity is the documented consumer (artifact DB); add the pointer comment citing the epic, matching the 13 idle CAD-KIND kinds precedent. ADD TESTS: E-TARGET-ID (bad nominal id via VALIDATE/DESCRIPTOR@) and E-TARGET-CAP (17th target or label-arena overflow) TTHROWS in target-test.f. Files: maki/onnx/import.f, maki/tensor.f(+test), maki/target/target.f(+test), maki/sched-key.f(+test), TRUSTED.md. Gates: touched suites + maki 90 + trust-lint/strict + error-code-lint.
