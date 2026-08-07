---
title: Publish a chain routine the engine cannot enter
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T21:13:37.794910+02:00"
---

The register calling convention (habu-pass-args-in-da01bd62) is blocked on this and measurement says nothing else blocks it. NPUB:REPUBLISH publishes only into a dictionary record, so every chain-published routine can be named, called and byte-copied by the engine, and the engine consults nothing on the way. Proven on master baee99e4 with tools/codegen-callsite-inventory.f and a probe: a chain-published routine of 20 bytes is copied verbatim into a later ORDINARY engine definition (that caller reports calls 0), and a 52-byte one gets a plain bl. Both answer correctly today only because the chain routine speaks the data-stack convention the engine speaks. So a routine may not adopt ANY other entry convention while it is an engine-reachable record, and a per-record convention field cannot fix it because the engine inliner reads only the record start and length. Needed: a publication path that emits code with NO dictionary record, reachable only by address from chain callers that read the callee contract, plus a refusal when anything tries to give such a routine a name. Acceptance: a routine published this way is unresolvable by the engine (search-wl miss, tick refusal), cannot be inlined by C-CALL because there is no record to copy from, and A64RAV refuses a register-convention call to a stack-convention callee and vice versa by name. Do NOT build convention adapters or two entry points; the seal is that the engine cannot name the routine at all.

Audit sizing (2026-08-07): the convention this blocks is 43% of the whole clang byte gap (356 of 820 bytes — entry/exit data-stack marshalling; ADD3/SQUARE-SUM/MAX-DIM/WIDE-ARITY are nothing else). The post-cut register/TOS-caching convention is the single biggest remaining prize and it waits on exactly this dot plus the cut.
