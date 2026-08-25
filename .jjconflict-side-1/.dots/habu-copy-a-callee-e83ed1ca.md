---
title: Copy a callee that has a control structure
status: open
priority: 3
issue-type: task
created-at: "2026-08-03T16:09:46.109315+02:00"
---

src/compiler/native/elaborate.f copies a callee's recorded body by staging its tokens where the call stood, which works because every token of a recorded body means a literal, an operation, a constant-and-operation word, a fixed value or a rename - no token builds a block. A callee with an 'if' or a loop in it is therefore not recorded at all (SPLICE-MEANING? answers false for 'control'), and test/compiler/native-inline.f measures a case where the SIZE rule admits the callee and only its shape refuses it: ': NINL-CTRL ( n -- n ) dup 0 < if drop 0 then ;' is ten instructions, exactly the ceiling, and is called rather than copied. What stops it is the skeleton: SKELETON walks the caller's tape once, counting the blocks each control word makes, and writes a forward join ordinal per token; a spliced body that made blocks would need its own counts merged into that walk at the call token, and every ordinal after it shifted. What to build: have the inline decision, which already runs before SKELETON, ask each recorded body how many blocks it makes and have SKELETON add that at the call token - one more number per token in the same table - then have the splice open and close those blocks. The record already holds what is needed; it is the two walks that have to agree. Owners: NELAB, NINL.
