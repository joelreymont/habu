---
title: Bring every engine JSON record into the diagnostic contract
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T23:30:47.569389+02:00"
---

Problem (lane 378 r4-contract, ec8978cc): four src/core/render.f JSON records have shapes or repair classes tools/gate-json-assert-core.f GJA-DIAG-SHAPE, tools/repair-packet-core.f and docs/repair-diagnostics.md do not know: E-BAD-STORED-SIGNATURE (:1109), W-EFFECT-NOT-RECORDED (:1183, no repair_class or suggestion), E-USING-SHADOW-GLOBAL / disambiguate_using_shadow (:1241), E-SHADOWED-ARITY / match_shadowed_private_effect (:1288); DIAG-CONTRACT fails on each as it did on E-TRUST-UNRESOLVED. Also the code-to-shape mapping is duplicated in GJA and RP. Acceptance: each record emitted through tools/check.f --json-errors passes DIAG-CONTRACT and builds a packet (or the record is changed to a documented shape, W-EFFECT-NOT-RECORDED gains its class or is stated as a warning outside the contract); one code-to-shape table both tools read; seen failing first. Files: src/core/render.f, tools/gate-json-assert-core.f, tools/repair-packet-core.f, docs/repair-diagnostics.md.
