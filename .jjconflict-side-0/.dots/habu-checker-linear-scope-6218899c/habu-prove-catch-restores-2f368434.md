---
title: Fix catch argument and linear-owner contract
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T20:48:57.220131+02:00"
blocks:
  - habu-unify-all-quotation-56884608
---

DECISION FROZEN 2026-07-27 (orchestrator), and this dot is now the single owner of what `catch` promises. Measured reality wins: `catch` does NOT restore the caller's pre-call argument values on the throw path. docs/forth.md line 1104 currently claims "`catch` preserves the pre-call args under the throw code: `nv ' WORD catch` on a throw leaves `( nv code )`". That claim is false. Standard ANS catch/throw restores stack DEPTH with unspecified cell contents, and Habu's engine behaves that way; the documentation promises more than the engine delivers. The engine is not being changed to match the documentation; the documentation is being corrected to match the engine, and the code that trusted the documentation is being audited.

Measured on master through the real bin/hb load path, 2026-07-27, with no test harness involved. Fixture:

    -4401 constant E-PROBE
    : B3 ( n -- n ) {: v:n :} v 0 > if E-PROBE throw then v ;

Throwing case, `77 [: B3 ;] catch`, leaves ( -4401 -4401 ): the throw code is in the result cell AND in the cell that held the argument 77. The 77 is gone. Non-throwing control, `-5 [: B3 ;] catch`, leaves ( -5 0 ): the argument cell correctly holds the quotation's returned value and the code is zero. So the clobbering is specific to the throw path, and what it writes over the argument cell is the throw code. This is the fixture the vector-seal lane called B3, reproduced independently here rather than inherited.

This cost real work: the first VEC:SORT! released its exclusive-mutation seal through a pointer it read back out of a post-catch argument cell, and crashed. The workaround still in the vecmem lane is the bare `2drop` at lib/vector.f:444 inside SORT!, commented "discarded: see the note above" - in-lane only, not on master.

FOUR OWNED OUTCOMES ON THE ARGUMENT CONTRACT:

(1) Documentation correction. Rewrite docs/forth.md line 1104 to state the real contract: after a caught throw only the stack DEPTH is restored, and every cell the quotation's row covers may hold an arbitrary value - in practice the throw code. State positively what a caller must do instead: carry nothing across `catch` that it needs afterwards, and pass no argument it intends to read back. lib/memory.f WITH-BYTES already follows that discipline by passing no arguments, so name it as the documented pattern rather than leaving it looking like an accident.

(2) Audit. Sweep every `catch` site in the repository and report each one that reads an argument cell after the caught call, separating sites that merely discard the cell (safe) from sites that use its value (broken). The audit produces a table, and every broken site is either fixed in the same change or given its own leaf with the fix named.

(3) Regression. Pin the B3 fixture, both the throwing and the non-throwing case, so the corrected contract cannot silently drift back. It runs through the ordinary checked load path, not through a copied model of catch. Note that this fixture needs a test-range error constant: master's lib/errors.f reserves -4400 through -4499 for test infrastructure and defines only E-TEST-CAPACITY at -4400, so -4401 is free and the constant the lane called E-TEST-PROBE has to be minted as part of this work.

(4) Resolve the VEC:SORT! workaround. The commented `2drop` at lib/vector.f:444 is either deleted, if the corrected discipline makes it unnecessary, or kept with a comment that cites this decision instead of citing an undecided question. That resolution lands with, or immediately after, the vector lane's integration, since the site only exists there today.

RETAINED FROM THIS DOT'S ORIGINAL CONTRACT, and now grounded by the decision above rather than contradicted by it - the checker side of the same question. RSCATCH currently unifies only a quotation's normal output with its input. If a throw path consumed, disposed, or deconstructed a linear value, the checker still restores the old stack rows and certifies a live owner that no longer exists; and the measurement above shows the underlying cells are not preserved either, so the certified owner is doubly forged. Required result: when a caught quotation can throw and its entry data or return row contains a linear value, RSCATCH must prove the recorded exceptional rows equal the quotation entry rows with the same nominal linear identities and multiplicities. A mismatch rejects at catch; non-linear catch behavior and a quotation with no throw remain unchanged. Do not add runtime guards and do not special-case MATCH by spelling. Owner: the catch linear-restoration rule in src/core/checker.f. Dependency: habu-unify-all-quotation-56884608, recorded above.

Acceptance, both halves. Argument contract: the documentation and the engine agree, the audit table exists and every broken site named in it is resolved, the B3 fixture is pinned in both directions, and the VEC:SORT! workaround is deleted or kept with a citation to this decision. Linear owners: negative checked regressions reject owner-drop-then-throw, dispose-then-throw, return-stack abandonment, and MATCH payload consume-then-throw, while a stack-preserving body throw still accepts and branch order cannot change the verdict. Use the real CHECK! and bin/hb path and the existing type-linear and type-match providers.

SUPERSEDES habu-decide-catch-arg-eeb239e4, which posed the same question as an undecided choice and has been deleted. The decision now lives here, so catch semantics have one owner. Priority raised from 2 to 1 to match the superseded dot, because a false documented guarantee about catch has already produced one crash.
