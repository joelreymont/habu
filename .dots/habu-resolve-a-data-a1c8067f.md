---
title: "Resolve a data word's address in the native chain"
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:58:25.944987+02:00"
---

The HIR source-word model now carries a 'fixed' meaning: a word that pushes one value, which is what a create-d data word does. The value is STATED by whoever builds the word model (test/compiler/native-source-fixture.f NSRC:MODEL-DATA, tools/codegen-compare-chain.f), because the chain cannot yet look a data word's address up in the engine's dictionary. Wanted: the elaborator resolves a name the word model does not declare by asking the engine what that word is - a data word answers its address - so tools/codegen-compare-corpus.f CELL-BUMP compiles from its own spelling with nothing told to the harness. Also the AOT half of it: a published routine holding a raw process address needs a relocation, which is why this is a capability and not a one-line lookup.

Scout update (2026-08-05): still real and THE one hard capability blocker for the cut — every create'd data word in the engine's own source hits it (migrate.f:288-290, hir-word.f:589-593 still state the parked-address seam). Path repair: the cited tools/codegen-compare-chain.f is now tools/codegen-compare-migrated.f (CELL-BUMP still hands BUMP-ADDR to DEFINE-DATA at :83-90). The AOT half (a published routine holding a raw process address needs a relocation record — the new publisher owns relocation, so the record kind lands there) is untouched.
