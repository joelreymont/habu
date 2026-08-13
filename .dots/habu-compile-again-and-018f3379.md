---
title: Compile again and leave
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-02T12:29:04.448224+02:00\""
---

The native dialect now compiles begin/until and begin/while/repeat (src/compiler/native/hir-word.f DEF-CONTROL, src/compiler/native/elaborate.f DO-WHILE and DO-CLOSE-REPEAT). Two loop words of the same family are still unmodeled and refused as E-HIR-UNMODELED: `again`, which closes a begin loop with an unconditional back edge and no exit at all, and `leave`, which leaves a counted loop from the middle. `again` needs a rule for a loop whose exit block has no predecessor unless a `while` gave it one - the block after the loop is unreachable when there is none, and the elaborator has no way to say unreachable; `leave` is a branch to the counted loop's join carrying the live values, which is the same stub-and-join construction `while` uses, plus the frame bookkeeping for finding the innermost counted loop that DO-FRAME already does. Both want the acceptance shape of habu-compile-while-repeat-c8ed5268: a corpus exemplar compiled from source through the engine's reader and executed against the interpreted word.

Claim: agent=againleave workspace=.jj-ws/habu-again-leave
