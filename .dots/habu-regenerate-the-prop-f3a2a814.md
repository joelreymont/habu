---
title: Regenerate the prop-test recipe ledger
status: closed
priority: 3
issue-type: task
created-at: "2026-07-26T09:02:20.569994+02:00"
closed-at: "2026-08-02T15:17:47.146157+02:00"
close-reason: "Superseded by the a8c716c5 hard cut: the AXR recipe ledger and registry metadata were deleted; real primitive behavior remains covered by ordinary PRIM-PROP tests at test/prop-test-core.f:509-513."
---

Problem: the AXR recipe ledger at the end of test/prop-test-core.f (one audited recipe line per live primitive-effect row, parsed by the AXR package from its own source) is maintained by hand; adding a primitive means hand-writing a recipe line that restates row identity, and the TEST-REAL-REGISTRY-FILES live-axiom ratchet (322/4 at the raw-primitive delivery) must be updated with the causing axiom at the same time. Required result: a checked Habu regenerator tool that emits fresh ledger recipe lines from the live primitive-effect table, preserving the audited proof-kind field of existing rows and flagging rows whose identity changed rather than silently rewriting them, so a primitive addition regenerates its recipe instead of hand-copying identity fields. Acceptance: running the regenerator on the clean tree is a byte-identical no-op; adding a synthetic row in a fixture produces exactly one new recipe line with identity fields matching the live table; a changed existing identity is reported, not rewritten. Files: a new tools/ regenerator, test/prop-test-core.f only as its data target. Verify: the prop-test suite before and after a no-op regeneration. Depends: none. Ownership: ledger regeneration tooling only. Claim: unassigned.
