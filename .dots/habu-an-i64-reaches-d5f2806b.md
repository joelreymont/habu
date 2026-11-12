---
title: An i64 reaches a u8 input through any word declared n
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T13:32:12.588988+02:00"
---

Problem: ': ID ( n -- n ) ;  : U8C ( u8 -- ) drop ;  : X ( i64 -- ) ID U8C ;' is accepted (program b3 in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/join): n passes integers in both directions (src/core/checker.f INT-WIDENS? :3257-3258, CON-OK? :3296-3297; docs/forth-card.md:125-127 says so), so any word declared n launders a wide value into a narrower declared input. The same pass-through makes a join unsound: CON-OK? answers true for n against any integer type and binds nothing, and CF-JOIN-D (:20037) answers the first row, so ': U8C ( u8 -- ) drop ;  : X ( i64 bool -- ) if drop 5 else then U8C ;' hands an i64 to U8C, accepted in one arm order (hunt /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/join-hunt.md). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed".

Rulings, Joel 2026-11-10: he chose "Narrowing converts": any integer passes to n; n into a fixed-width input needs a conversion, fitting literals excepted. He chose "i64 is fixed (Recommended)": i64 is fixed-width for the rule; n into i64 needs `>I64`; n into addr is refused. On the design's undetermined facts: "so determine it!".

Design: /Users/joel/Work/reviews/habu-10x-2026-11-10/designs/n-rule-design.md, section 5. A join of n with a fixed-width t gives n; at a loop, entry n with back edge t is sound and entry t with back edge n is the narrowing (CF-CARRIED= :20077). The conversion words and the first literal-fact commit are on master. Open, in order:
- Lane 2, literal facts: [char], constants and a local's declared type (LOC-SCALAR-BIND binds an annotated local as an input with the cell's fact, so '300 {: c:u8 :}' is refused); then the top level (the saved top-level row carries the fact in its scratch push nodes). The fact is dropped at loop entry.
- Lane 3, tree migration by subsystem; the census of sites is under /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/ncensus. 30 x86-64 files stay unchecked past src/os/linux-x86-64/elf.f:101 (E-UNDEFINED MSIZE, habu-re-enable-the-959502b5).
- Lane 4, the rule. Its brief carries: ': COUNTED ( n -- n ) {: c:n :} 0 c [: 1+ ;] TIMES ;' and an EACH over an n-length local stay accepted; with APPLY1R ( [ a -- ] a -- ), '[: U8C ;] 300 APPLY1R' is accepted where '300 [: U8C ;] APPLY1' is refused, because the literal binds a first and the declared row holds no fact; the literal fact is cleared rather than the refusal skipped, so '( u8 -- ) 65 swap EQ2' would be refused as a narrowing while '65 EQ2' is accepted; ': X ( u8 -- ) 300 swap P1 U8C ;' and ': X ( u8 -- u8 ) 300 [: PICK1 ;] KUP ;' hand 300 to a u8 on every engine. The withdrawn join fix, unlanded commit ulokwsyz 95d8e7a8 "Join n with a fixed-width integer as that type", is retired by this lane.
Found outside the rule by the design: c, declares n while c! declares u8 (src/habu/prims.f :550, :522); a raw variable carries an i64 into a u8 word (the raw-cell rule, checker.f header "Raw cells" :99).

Acceptance: every program in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/join and cases8/join-review (results in table.txt, types.txt and c.txt) that passes a wider or unknown-width value to a narrower declared input is refused, in either arm order: b3, the seven programs of the join hunt (it2, ie1, cs1, wr1, dl1, lb2, x1; cases8/join/w with U8C taking the i64), b/na7.f, d/c3.f and c3b.f (300 and an i64 reaching ( u8 -- ) through a loop); c/rs3.f (the return-row walk) and d/c2body.f (a u8 reaching an addr input inside a loop body) decided and pinned; a cell that is jbox<n> on one path and jbox<i64> on the other, or a quotation whose row holds n on one path and i64 on the other (cases8/join-review/jf), refused in either arm order; a literal that fits accepted and 300 into u8 refused; each pinned by a test through the real load path.

Depends: none. Claim: unassigned.
