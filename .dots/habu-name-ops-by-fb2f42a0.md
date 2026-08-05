---
title: Name operations by shape, not index
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T17:53:41.287289+02:00"
---

test/compiler/native-elaborate.f BUMP-BODY (and siblings) pin operation identity by ABSOLUTE index (m blk 7 F-OP, count pinned at 10), so any legitimate optimization that changes the op count reads as an IR invariant violation — this mis-directed an entire lane into a nonexistent 'bound bug' (E-IR-FUN-BOUND has 20+ throw sites; the raiser was the test itself). Rewrite the fixtures to assert by operation shape/kind sequence or by searching for the named op, so the suite constrains semantics, not op arithmetic. Ships together with the literal-CSE landing (habu-fold-constants-and-cbe4e25e) whose op-count change is what exposes it.

Claim: agent=remat2 workspace=.jj-ws/habu-fold-constants-and-cbe4e25e
