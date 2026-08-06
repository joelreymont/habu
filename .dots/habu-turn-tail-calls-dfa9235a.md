---
title: Turn tail calls into jumps
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.347021+02:00"
---

A routine whose last operation is a call pays bl + frame + ret where a b would do: reuse the caller's frame, jump, let the callee return to the original caller. Forth words end in calls constantly, so this is disproportionately large for call chains (A->B->C->D shapes) and cheap: the elaborator already knows when a call is the final operation with results passing straight through; selection emits the tail form; the verifier refuses a tail jump whose stack shape differs from a return (same out-arity, pointer at the return position — the placement machinery already computes both). Watch NREACH: a redirected tail jump is still a branch into the old routine — the site decoder must recognize b-as-call targets or redirect will miss them; extend NBR/NWALK accordingly with tests. Acceptance: the call-chain corpus row (A-to-D) loses its intermediate frames, measured bytes and ns both drop, answers identical, redirect still finds and moves tail sites. Sequence AFTER the clang column so the win is measured against the reference.

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6 (measure on the production compiler after the cut). Re-scoped by the codegen review: do NOT extend NREACH — it is deleted; replace a final shape-compatible call+return with a direct branch only once every compiled routine uses the new compiler, proving the stack/result convention at the site.

Claim: agent=tailcalls workspace=.jj-ws/habu-turn-tail-calls-dfa9235a

UNBLOCKED (2026-08-05, user order): the optimization program proceeds NOW on the chain as it stands — the hard cut continues in parallel and is no longer a prerequisite. Standing acceptance for every optimization lane: name the corpus rows expected to improve BEFORE implementing; show the emitted instruction delta on them; every oracle answer preserved bit-for-bit incl. NaN; report BOTH gaps per touched row (chain-vs-clang closed, chain-vs-own-baseline gained) from tools/codegen-compare.f; re-pin the chain baseline with --update-chain only after the report is read; no regression on untouched rows outside a stated multi-objective trade. New instruction forms (madd/msub, ldp/stp, bitfields, ccmp, NEON) require Rocq rows in formal/Common/Insn.v with enc/wf/roundtrip before the emitter uses them — the CG-02 discipline, applied per-lane not deferred.
