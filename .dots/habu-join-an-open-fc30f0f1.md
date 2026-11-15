---
title: Join an open-width option with a known one at THEN
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.748522+02:00"
---

Problem: ': M3 ( bool -- n ) {: f:bool :} depth {: d:n :} f IF OPTION:NONE ELSE 1 2 PR:MAKE OPTION:SOME THEN {: c :} depth d - ;' (PR a two-field STRUCTURE) is refused at THEN: 'expected: option<pr> @option.slot2<pr> actual: option<a>'. The refusal is LOGHID-JOIN?'s equal-width guard (src/core/checker.f), which joins a logical cell with an expanded group only at equal width. Same refusal before and after c9743c5a (stuart's fold 1 probe m.f, 2026-11-07). Fails closed. Acceptance: a branch join of an open-width option with a known-width one resolves the width and compiles; M3 returns depth 1 for both flags; a case in test/wide-typed-local-probe.f. Files: src/core/checker.f, test/wide-typed-local-probe.f. Verify: the case above. Depends: none. Ownership: carl. Claim: unassigned.

Finding 2026-11-11 (carl's worker, not Joel's words): the refusal comes from CF-ARMS-DEPTH's ROW-CELLS compare before the join (an open argument measures one cell: 2 against option<pr>'s 3), and it keeps the code correct. UNIFY refuses to bind an open payload variable to a value wider than one cell (PARAM-BIND-OK?, U-TYPE LAYOUT-BLOCK? outside transport mode); OPTION:NONE {: r:option<pr> :} and a call taking option<pr> are refused on master the same way. Forcing the bind certifies wrong code: the IF arm's OPTION:NONE pushed 2 cells and nothing re-derives that width when the join binds the argument, so 77 true M3 eats the 77. A loop back edge shows the same refusal at REPEAT. A fix needs a design: the pushed width of an open-width call follows a later binding, or the arms are widened at the join (codegen or width facts). Until then the message could say why the depths differ. Not ready: needs that design.
