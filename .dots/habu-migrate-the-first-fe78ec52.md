---
title: Migrate the first compiler entry to the new chain
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-31T18:20:04.196248+02:00\""
---

The emission adapter landed (commit 232a43def967: src/compiler/native/emit.f, package A64EMIT), so the native chain now runs source tape through typed IR, selection, allocation, acceptance, and bytes - but only tests drive it. The residue of dot habu-lower-native-emission-cbc7f99b is the production seam that dot also owned: pick one real compiler entry point (the first checked colon definition the engine itself compiles), route it through the new chain end to end, and make it structurally unable to bypass validated A64IR - the emitter refuses a module the validator has not accepted, so the entry must have no other door to bytes. Includes whatever object-record construction (design section 7.11) that first entry actually needs and nothing more. Depends on the calling-convention seam habu-bind-arm64-arg-f76afa3a for words that take or return values through the data stack.

Claim: agent=entrymigrate workspace=.jj-ws/habu-migrate-the-first-fe78ec52
