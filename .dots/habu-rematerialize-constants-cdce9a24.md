---
title: Rematerialize constants instead of spilling them
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:41:33.933692+02:00"
---

Constant rematerialization in the allocator: when an evicted class carries a constant whose materialization chain is length 1, re-emit the movz at use sites instead of store/reload — a cost COMPARISON (re-emission no dearer than the reload), never a blanket rule; a materialised constant at the allocator is a tied movz/movk chain up to 4 long (a64ir.f:251-259) and BIG-CONSTS (corpus 4) is the named regression fixture blanket remat would break.

Grounded design (verified sites): bind MOVZ/MOVK via BIND-DIALECT/SAME-SYM?; split CL-SLOT's double meaning (real slots only) with a per-class remat mark and one shared CL-EVICTED? for the five reader sites (MB-SPILLABLE?/MB-FRAMED?/MB-DUE?/MB-FINISH/MB-PLAN-*); MB-EVICT sets the mark and skips NEW-SLOT (frame.f untouched); MB-PLAN-LOADS plans P-REMAT; spill.f re-emits; the validator re-derives per site.

DEFERRED behind two decisions: (1) the spill rewrite loop must reach production first (fits-or-refuses today — see the cut dot); (2) the validator has no link from a remat site to the class's constant (FLOW-CK ties reload to store via the slot attribute; a wrong-immediate movz is well-formed) — candidates: class identity on the op (weakest — validator would trust the plan), handing VERIFY the pre-spill module (orchestrator's leaning: honest independent re-derivation), or all-movz-to-one-reader-agree. The acceptance's mutation test is impossible until the link exists. Shares one lane with habu-rematerialize-the-loop-1faad3e1 (loads second).
