---
title: Scope clobber-lint locals across multi-while loops and quoted keywords
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T08:10:27.048617+02:00"
---

Problem: the clobber lint's local walk (tools/lint/clobber-lint-core.f DEF-LOCALS, SCOPE-TOKEN; lane sealed fold g) pops one block per closer from BLOCK-CLOSE?'s list, so in `begin … while … while … repeat … then` the `then` pops the block enclosing the loop and drops its live locals; a later use of one reads as a call. A keyword after `postpone`, `'`, `[']` or `[char]` still counts as an opener or closer. No file the lint scans has either shape today (review /tmp/jerry-sealed-rv4-tmp, claim 6); the walk's opener and closer lists equal src/lib/source-syntax.f's minus match and quotations, so source-syntax.f may share the multi-while fault. Acceptance: measure on bin/hb which locals are live after each closer of a two-while loop, make the walk (and source-syntax.f if it differs from the engine) agree, a fixture per shape asserted in tools/lint/clobber-lint-test.f failing before. Verify: the clobber-lint row. Depends: lane sealed landed (habu-answer-tools-check-745b0ccd). Claim: unassigned.

Also (review /tmp/jerry-sealed-rv5-tmp, before fold i too): a value left under an instruction's operands and bound by a group after it, `9 15 SP 0 STR, {: k:n :}`, is read as the STR's first register (1, true 0); no engine file has the shape. Fixture and fix belong with the walk above.
