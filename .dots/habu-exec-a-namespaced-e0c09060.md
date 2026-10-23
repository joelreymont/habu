---
title: Execute a namespaced generated constructor with a same-tail name
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T12:19:03.817240+03:00"
---

Problem: test/type-ctor-suite.f executes top-level generated constructors and asserts their payloads (CLOPT:SOME/NONE, CLFC-RT-*, CN-RT-*, CND-RT-*), but three cases are never executed: a constructor whose tail is spelled like a word in scope (zwv publishes ZWV:DUP and ZWV:SWAP; CN10/CN11 only check `construct zwv dup|swap`, LK1/LK2 only check that bare dup/swap still bind the prims); a constructor of a family declared inside a package (ZMK-YES is only defined, CP3 only checks CNPUB-CNRES:YES); and rejection of a same-tail type from another package. Residue of closed dot habu-exercise-publication-of-ddd5c76f, whose superseding campaign dot habu-campaign-c3-the-a2477c89 no longer exists. Coverage gap, not an established runtime defect. Acceptance: in test/type-ctor-suite.f, execute ZWV:DUP and ZWV:SWAP and a constructor of a family declared inside a package, through the real load path, and assert the constructed variant and payload; declare a same-tail family and constructor in another package and show the checker rejects passing its value where the first is expected. Touch the generated-constructor publication code only if this behavior fails, at the responsible layer. Files: test/type-ctor-suite.f. Verify: the WHITEBOX-SUITE row for test/type-ctor-suite.f (unsealed engine from test/whitebox-engine.f, <unsealed engine> --load test/type-ctor-suite.f). Depends: none. Ownership: test/type-ctor-suite.f. Claim: unassigned.
