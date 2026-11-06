---
title: Join an open-width option with a known one at THEN
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.748522+02:00"
---

Problem: ': M3 ( bool -- n ) {: f:bool :} depth {: d:n :} f IF OPTION:NONE ELSE 1 2 PR:MAKE OPTION:SOME THEN {: c :} depth d - ;' (PR a two-field STRUCTURE) is refused at THEN: 'expected: option<pr> @option.slot2<pr> actual: option<a>'. The refusal is LOGHID-JOIN?'s equal-width guard (src/core/checker.f), which joins a logical cell with an expanded group only at equal width. Same refusal before and after c9743c5a (stuart's fold 1 probe m.f, 2026-11-07). Fails closed. Acceptance: a branch join of an open-width option with a known-width one resolves the width and compiles; M3 returns depth 1 for both flags; a case in test/wide-typed-local-probe.f. Files: src/core/checker.f, test/wide-typed-local-probe.f. Verify: the case above. Depends: none. Ownership: carl. Claim: unassigned.
