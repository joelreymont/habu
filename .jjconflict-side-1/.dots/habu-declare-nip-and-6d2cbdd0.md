---
title: Declare nip and rot as HIR renames
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T23:20:40.124069+02:00"
---

Full context: docs/compiler-ir-design.md section 7.3 names NIP and ROT as stack renames that produce no operation, but src/compiler/native/hir-word.f REGISTER-WORDS declares only dup/drop/swap/over - and the native-hir suite currently uses rot as its undeclared-word NEGATIVE fixture (UNDEC-CASE), so declaring it flips that fixture. Add nip (consumes 2, puts back 0) and rot (consumes 3, puts back 1 0 2 - verify against the design), re-point UNDEC-CASE at a genuinely unmodeled spelling, extend the elaborator suite with a rot-using word asserting zero added operations. Before the optimizer leaf assumes the full rename set.

Claim: agent=renames workspace=.jj-ws/habu-declare-nip-rot (RELEASED 2026-08-21: workspace gone, no live lane - gc)

Done: REGISTER-WORDS now declares nine words instead of seven. `nip` consumes
two values and puts back only the one that was on top, so its pick list is the
single depth zero. `rot` consumes three and puts all three back rotated; the
list is derived at the head of hir-word.f - a b c are depths 2, 1 and 0, `rot`
leaves b c a, picks are listed bottom first, so the list is 1 0 2. The pick-cell
ceiling went from seven to eleven, which is the six renames' picks added up.

Both orders are proved rather than described. The elaborator suite gained
`: ROT3 rot - ;` and `: NDIF nip - ;`, each asserting two operations - the
subtraction and the return, nothing for the rename - and asserting exactly which
block argument is which operand of the subtraction. Subtraction's operands are
not interchangeable, so every skewed pick index reds: nip's one pick, each of
rot's three, and both neighbouring rotations (`-rot` and leaving the values
alone) were mutated one at a time and every one of them failed both the
elaborator suite and the word-model suite. Understating the pick-cell ceiling
also fails, so the committed ceiling is checked and not decorative.

The undeclared-word negative fixtures no longer use `rot`, which is now modeled.
They use `xor`: the dialect's five opcodes are a closed family with no bitwise
operation, so modeling `xor` would take a new opcode, elaboration and lowering,
while a new stack word is only another rename row. That keeps the fixture
testing an undeclared word as the rename vocabulary grows. Nothing outside the
word model and the elaborator reads this vocabulary, so the selection suite was
unaffected and stays green.
