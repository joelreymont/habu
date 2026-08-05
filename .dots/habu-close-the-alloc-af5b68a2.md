---
title: Close the allocator claim so only the validator reads it
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:11:06.396109+02:00"
---

src/compiler/native/regalloc.f says in its own header that a consumer wanting to emit code must go through src/compiler/native/regalloc-verify.f, because 'an allocator that certified its own output would be checking its belief against itself'. But A64RA:CLAIM@, DEF@, LAST@, VALUES, POOL, MODULE@, MOVK-SYM and GPR-TYPE@ are all PUBLIC, so the discipline is a convention rather than a structure: any package can read an unvalidated claim by name. Evidence: mutating src/compiler/native/emit.f's REG-OF from A64RAV:REG@ to A64RA:CLAIM@ leaves the whole emission suite green, because the emitter's own ALLOC-CK independently establishes acceptance, freshness and module identity first. The mutation is caught by review and not by any test, which is exactly the shape of defect a structural seam should make unwritable. Wanted: a capability the validator holds and no one else can mint - for example A64RA publishes its claims only to a holder of a token A64RAV alone can obtain, the way HIR-WORD's 'interned' proof token forces its declarers to check first (LESSONS.md). A64RAV needs DEF@/LAST@/CLAIM@/POOL/MODULE@/VALUES/MOVK-SYM/GPR-TYPE@; test/compiler/native-regalloc.f reads CLAIM@ and DEF@/LAST@ directly and would move to the token or to the validator's readers.
