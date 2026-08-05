---
title: Spill for real instead of refusing
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.396651+02:00"
---

Close the declared CALL-PRESSURE gap (E-A64RA-SPILL at 7 live values across a real call) and its class: live-range splitting at call boundaries (values dead across the call keep registers; values live across it get split ranges with spill/reload at the boundary the residency machinery already prices), plus rematerialization of constants and cheap pure ops instead of stack traffic. The allocator's hull intervals stay; the splitter runs when the pool check would refuse today. Verifier re-derives every spill decision (a reload of a value whose cell is stale, a spill nothing reloads — both refusals). Acceptance: the corpus4 gap declaration comes OFF (the coverage check will demand it), L7-class shapes compile and win against the engine, no existing row regresses.

NOT post-cutover: CG-28 requires complete spill/control support BEFORE production cutover — this dot is a prerequisite of habu-cut-colon-compilation-a5aa3f1f (full-language cut). Split live ranges at calls, spill only values live across the boundary, rematerialize constants/cheap pure expressions; closes the PRESSURE-LOOP and CALL-PRESSURE corpus gaps.

UNBLOCKED (2026-08-05, user order): the optimization program proceeds NOW on the chain as it stands — the hard cut continues in parallel and is no longer a prerequisite. Standing acceptance for every optimization lane: name the corpus rows expected to improve BEFORE implementing; show the emitted instruction delta on them; every oracle answer preserved bit-for-bit incl. NaN; report BOTH gaps per touched row (chain-vs-clang closed, chain-vs-own-baseline gained) from tools/codegen-compare.f; re-pin the chain baseline with --update-chain only after the report is read; no regression on untouched rows outside a stated multi-objective trade. New instruction forms (madd/msub, ldp/stp, bitfields, ccmp, NEON) require Rocq rows in formal/Common/Insn.v with enc/wf/roundtrip before the emitter uses them — the CG-02 discipline, applied per-lane not deferred.
