---
title: Pass arguments in registers between native routines
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.358771+02:00"
blocks:
  - habu-publish-a-chain-c4dbf1d3
---

The structural gap against LLVM: every call between chain-compiled routines still crosses the data stack in memory. Build an internal register convention — arguments and results in x0..x7 / d0..d7 by position, data-stack form only at engine boundaries (engine-compiled callers/callees, EXECUTE, anything the checker cannot see through) — and let the publication seam record which convention each routine speaks so call sites select the matching form; NREACH redirects only between matching conventions or through an adapter thunk it emits. The clobber records already prove per-routine register facts; the residency/placement machinery already knows what crosses; the verifier re-derives the convention per site (args in the right registers at the bl, results read from the right ones after) exactly as it re-derives dstack discipline today. This is the largest single expected win on call-heavy rows and the most dangerous change in the program — it lands LAST of the scalar work, after the clang column has priced it and TCO has simplified the call shapes. Depends: habu-turn-tail-calls (sequencing), the clang column (pricing).

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: after the hard cut the old compiler is gone, so build NO convention adapters or per-record compatibility tags — one internal register convention, with explicit data-stack conversion only at true engine/foreign boundaries.

Claim: agent=regargs workspace=.jj-ws/habu-pass-args-in-da01bd62

MEASURED FIRST (2026-08-06, regargs, master baee99e4). The instrument is
`tools/codegen-callsite-inventory.f` (+ `-run.f`, suite
`test/compiler/codegen-callsite-inventory.f`, gate row
`compiler-codegen-callsite-inventory`). It counts the instructions that exist
only to move arguments and results through the data stack, split by WHERE they
sit, because the two kinds are removed by different things:

  site  the marshalling in the run beside a call. This is the convention's prize.
  own   the routine reading its own arguments at entry and publishing its own
        results at exit. This is the engine boundary.

Over all 54 migrated corpus rows: 701 instructions, 6 calls, **site 10, own 115**.
Only 5 rows carry any `site` at all, and 3 of the 10 are one self-call (FACT-N).

Why `site` is so small, which is the finding and not an accident:

1. A tail call passes its arguments by LEAVING THEM WHERE THE CALLEE ALREADY
   READS THEM, so it marshals nothing. TAIL-BIG-N, TAIL-MID-N, TAIL-CHAIN-N and
   TAIL-PAIR-N are ONE instruction each (4 bytes) — the tail-call lane already
   deleted the whole inter-hop round trip this dot was scoped to delete.
   TAIL-CHAIN (A->B->C, the row this lane was told to predict a win on) has no
   stack round trip between hops left to remove.
2. A non-tail call marshals only what the caller COMPUTES. NONTAIL-N passes its
   argument straight through and its only `site` instruction is the load of the
   RESULT after the `bl`; TAIL-AFTER-N makes a real call and a tail call and
   carries no data-stack traffic at all.
3. The chain's own inliner already removed most chain-to-chain calls outright:
   CALL-FAN-N, CALL-FAN-BIG-N, CALL-LOOP-3-N, T-GET-N and VEC-COPY-CELLS-N all
   report calls 0.
4. A best case built ON PURPOSE — a caller that computes BOTH arguments and
   calls a 16-instruction callee, non-tail, inside a loop — carries site 2.

THE `own` 115 IS NOT REACHABLE, and this is the blocker rather than a cost
question. `NPUB:REPUBLISH ( ptr u8 n n -- )` publishes only INTO A DICTIONARY
RECORD; there is no code-only publication path. So every chain routine can be
named, called and copied by the engine, and the engine consults nothing on the
way. Proven on this tree, not read off the inliner: a chain-published routine of
20 bytes is byte-copied verbatim into a later ORDINARY engine definition (that
caller reports calls 0), and a 52-byte one gets a plain `bl`. Both answer
correctly today only because the chain routine speaks the data-stack convention
the engine's own code speaks.

Therefore, under this leaf's binding re-scope — one convention per routine,
stated by the contract, carried by the record, NO adapters and NO per-record
compatibility tags — no routine can adopt the register convention today: the
engine would splice its bytes into, or branch to it from, a caller that put the
arguments on the data stack, which is a wrong answer with no diagnostic. A
per-record convention field cannot close it, because the engine's inliner reads
only the record's start and length.

And at every engine boundary the convention is a strict LOSS. A
register-convention callee cannot be tail-branched to from a data-stack-
convention caller, because the result would come back in a register its own
caller does not read. TAIL-BIG-N would go from one instruction to a load, a
call, a store, a frame and a return.

WHAT THIS LANE DID NOT DO, deliberately: it did not build the convention, and it
did not add corpus rows for a convention that cannot be built. A real shape for
the rows exists when the blocker lifts — `MHA-QKVIDX`/`MHA-FLATIDX`
(`maki/mha.f:165-189`), ~24-operation pure-integer index helpers at arity (2->1)
and (1->1), each called from two `?do` loops with a computed argument, past both
inliners.

BLOCKED BY: the missing publication capability, dotted separately. The prize when
it lifts is dynamic and not static — `own` traffic executes on EVERY call, so a
hot loop over a helper pays it per iteration even though it is 2-4 instructions
of code. Re-derive both columns on the tree of the day before building; a
measured floor decays.

UNBLOCKED (2026-08-05, user order): the optimization program proceeds NOW on the chain as it stands — the hard cut continues in parallel and is no longer a prerequisite. Standing acceptance for every optimization lane: name the corpus rows expected to improve BEFORE implementing; show the emitted instruction delta on them; every oracle answer preserved bit-for-bit incl. NaN; report BOTH gaps per touched row (chain-vs-clang closed, chain-vs-own-baseline gained) from tools/codegen-compare.f; re-pin the chain baseline with --update-chain only after the report is read; no regression on untouched rows outside a stated multi-objective trade. New instruction forms (madd/msub, ldp/stp, bitfields, ccmp, NEON) require Rocq rows in formal/Common/Insn.v with enc/wf/roundtrip before the emitter uses them — the CG-02 discipline, applied per-lane not deferred.
