---
title: "Resolve a require against its own file's root first"
status: active
priority: 2
issue-type: task
created-at: "2026-10-27T14:06:17.801706+02:00"
---

Problem: src/core/include.f:1178 SEARCH-ROOTS searches a relative require's owner root, then the working directory, then the engine root. An app file that asks for a Habu library its own tree lacks (require src/lib/fs.f) is captured by a same-named file in the working directory, giving a duplicate definition or the wrong code. Joel (via kevin, 2026-10-27): Habu resolves each require against the source root of the file that contains it before anywhere else, so Habu's libraries always load Habu's files and a consumer's files load its own, whatever sits in the working directory or the entry directory. Acceptance: a relative dependency searches its owner root, then the engine root; the working directory can never answer a request that either of those answers (removed from dependency search unless a real consumer needs it, then last); top-level session requires keep the working directory as their owner root; docs/forth.md Source roots and the card updated; regression cases in test/source-root-exe-test.f (decoy src/lib file in the working directory is not loaded for an app's Habu-library require; decoy in the entry directory is not loaded for a Habu library's own nested require). Files: src/core/include.f, test/source-root-exe-test.f, docs/forth.md, docs/forth-card.md, the nested-check child loader if it relied on the working directory. Verify: engine rebuild, test/source-root-exe-test.f, full suite, two-generation build. Depends: none. Ownership: carl-roots. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-roots
