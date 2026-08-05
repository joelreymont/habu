---
title: Pass arguments in registers between native routines
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.358771+02:00"
---

The structural gap against LLVM: every call between chain-compiled routines still crosses the data stack in memory. Build an internal register convention — arguments and results in x0..x7 / d0..d7 by position, data-stack form only at engine boundaries (engine-compiled callers/callees, EXECUTE, anything the checker cannot see through) — and let the publication seam record which convention each routine speaks so call sites select the matching form; NREACH redirects only between matching conventions or through an adapter thunk it emits. The clobber records already prove per-routine register facts; the residency/placement machinery already knows what crosses; the verifier re-derives the convention per site (args in the right registers at the bl, results read from the right ones after) exactly as it re-derives dstack discipline today. This is the largest single expected win on call-heavy rows and the most dangerous change in the program — it lands LAST of the scalar work, after the clang column has priced it and TCO has simplified the call shapes. Depends: habu-turn-tail-calls (sequencing), the clang column (pricing).

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: after the hard cut the old compiler is gone, so build NO convention adapters or per-record compatibility tags — one internal register convention, with explicit data-stack conversion only at true engine/foreign boundaries.

UNBLOCKED (2026-08-05, user order): the optimization program proceeds NOW on the chain as it stands — the hard cut continues in parallel and is no longer a prerequisite. Standing acceptance for every optimization lane: name the corpus rows expected to improve BEFORE implementing; show the emitted instruction delta on them; every oracle answer preserved bit-for-bit incl. NaN; report BOTH gaps per touched row (chain-vs-clang closed, chain-vs-own-baseline gained) from tools/codegen-compare.f; re-pin the chain baseline with --update-chain only after the report is read; no regression on untouched rows outside a stated multi-objective trade. New instruction forms (madd/msub, ldp/stp, bitfields, ccmp, NEON) require Rocq rows in formal/Common/Insn.v with enc/wf/roundtrip before the emitter uses them — the CG-02 discipline, applied per-lane not deferred.
