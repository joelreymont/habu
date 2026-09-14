---
title: Give successors per-target argument windows
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:39:33.666279+02:00"
---

2026-09-14 correctness boundary: multi-successor branches now refuse any
destination requiring arguments, because the current representation cannot
attribute operands per edge. A condition operand with argument-free targets
remains valid. Rebuilt SHA91ea98b7 passes both target-position rejection cases
and the accepted control after independent review. Per-edge argument windows
remain an open capability; the verifier no longer silently admits them.

Full context: MODEL GAP from agent irverify 2026-07-30. Successor ARGUMENT AGREEMENT (counts and types against the destination block's arguments) is only decidable for single-successor terminators today, because an operation's successor-argument cells are one flat window with no per-successor boundaries - a two-successor terminator's arguments cannot be attributed to a target. Needs a per-successor argument sub-window in the op row (IR-OP layout change, same pool-cell-length discipline commit 81af2a24 used for keyed attributes, with the frozen-body proof in formal/Common/Structure.v updated in the same change - it WILL refuse a shape that breaks the contiguity claim, that is the fixture doing its job), then the verifier extends argument agreement to every successor. Blocks full design-6.5 conformance; single-successor agreement already enforced.
