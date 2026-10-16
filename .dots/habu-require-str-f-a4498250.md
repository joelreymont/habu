---
title: Require string.f where its words are used
status: open
priority: 3
issue-type: task
created-at: "2026-10-16T08:34:57.419648+03:00"
---

Problem: docs/forth-card.md section 7 says every file requires its own dependencies; an engine-provided module such as src/lib/string.f resolves without a require, so a missing require never fails, and the card says to write it anyway. About 146 files under test/, tools/ and src/lib/ name string.f words (CONTAINS?, STARTS-WITH?, STR= ...) without require src/lib/string.f. Examples are test/aot-payload-unsupported.f and test/aot-payload-graph.f; many test files lean on src/lib/test.f's require. Acceptance: every file under test/, tools/ and src/lib/ that names a public string.f word requires src/lib/string.f in its own require block. A file whose same-named word is its own definition is not counted. Excluded: test/compiler/ and src/compiler/ (tier 1), and carl's chain files test/c2-memory-e2e.f, test/native-file-unit-e2e.f, test/c2-xml-consumer-e2e.f, test/gate-images-test.f and test/native-file-admission-e2e.f until that chain lands. Files: the census list. Verify: the census rerun finds none; the full native suite at landing. Depends: none. Ownership: require lines of the listed files. Claim: unassigned.
