---
title: "Resolve a data word's address in the native chain"
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:58:25.944987+02:00"
---

The HIR source-word model now carries a 'fixed' meaning: a word that pushes one value, which is what a create-d data word does. The value is STATED by whoever builds the word model (test/compiler/native-source-fixture.f NSRC:MODEL-DATA, tools/codegen-compare-chain.f), because the chain cannot yet look a data word's address up in the engine's dictionary. Wanted: the elaborator resolves a name the word model does not declare by asking the engine what that word is - a data word answers its address - so tools/codegen-compare-corpus.f CELL-BUMP compiles from its own spelling with nothing told to the harness. Also the AOT half of it: a published routine holding a raw process address needs a relocation, which is why this is a capability and not a one-line lookup.
