---
title: Give HIR a trap terminator
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:23:52.575343+02:00"
---

MATCH's mismatch edge needs a zero-successor terminator; HIR has only br/brz/return. ir/verify.f:630 already contemplates 'a terminator that names no successor', so hir.trap carrying an exit status is a first-class form of the existing substrate. Design ruling from the match lane, adopted: the operand is a family ORDINAL into one shared trap-message table, never inline message bytes - the engine copies 'hb: bad <family> tag' into EVERY match site (~64 bytes/site, measured: two-arm match 128 bytes at short family name, 156 at 28-char name), and the chain must emit ONE shared trap routine instead (saves ~19KB over the 303 sites). Rows-first: model row for whatever instruction reaches the shared routine, then encoder, then select/emit. Acceptance: a forged bad tag through an unchecked boundary exits ENGINE-ERROR:BAD-TAG naming the right family; the trap routine is emitted once tree-wide; verify.f accepts the zero-successor form; negative: a trap with a successor refused. Files: src/compiler/ir/verify.f, src/compiler/native/{hir,select,emit}.f, formal/Common/Insn.v if a new machine form is needed. Verify: native suites, insn-proof, full gate. Depends: none. Blocks: the MATCH lane. First consumer: MATCH's mismatch edge; second: case.
