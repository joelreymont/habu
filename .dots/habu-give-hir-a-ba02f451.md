---
title: Give HIR a trap terminator
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:23:52.575343+02:00"
---

MATCH's mismatch edge needs a zero-successor terminator; HIR has only br/brz/return. ir/verify.f:630 already contemplates 'a terminator that names no successor', so hir.trap carrying an exit status is a first-class form of the existing substrate. Design ruling from the match lane, adopted: the operand is a family ORDINAL into one shared trap-message table, never inline message bytes - the engine copies 'hb: bad <family> tag' into EVERY match site (~64 bytes/site, measured: two-arm match 128 bytes at short family name, 156 at 28-char name), and the chain must emit ONE shared trap routine instead (saves ~19KB over the 303 sites). Rows-first: model row for whatever instruction reaches the shared routine, then encoder, then select/emit. Acceptance: a forged bad tag through an unchecked boundary exits ENGINE-ERROR:BAD-TAG naming the right family; the trap routine is emitted once tree-wide; verify.f accepts the zero-successor form; negative: a trap with a successor refused. Files: src/compiler/ir/verify.f, src/compiler/native/{hir,select,emit}.f, formal/Common/Insn.v if a new machine form is needed. Verify: native suites, insn-proof, full gate. Depends: none. Blocks: the MATCH lane. First consumer: MATCH's mismatch edge; second: case.

TWO ACCEPTANCE CLAUSES ADDED 2026-08-10 (from the dead-path lane's probes,
both measured through the real chain): (a) a routine whose EVERY path is dead
has zero returns - regalloc.f:2004 MB-RET-ORD throws E-A64RA-SHAPE when it
finds none, so this dot must decide what "the block control leaves through"
means for an all-dead routine and pin it; (b) select.f:3801 DPLACE-RETURN's
one-return census must learn about trap blocks (a second hir.return as a
stand-in was probed and is refused there by name - the trap is not a return).
Also measured: the builder refuses an unterminated block at E-IR-BUILD-STAGE,
so the trap is the ONLY route; and the dead-path elaborator half (725fbaa0)
is blocked behind this dot - its measured prize is 41 definitions.
