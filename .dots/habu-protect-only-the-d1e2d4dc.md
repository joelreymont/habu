---
title: Protect only the words a private family generates
status: open
priority: 1
issue-type: task
created-at: "2026-09-18T12:50:13.903522+03:00"
---

Problem (adversarial review of the 2026-09-18 batch, probe p15, engine 3e56365e): TFAM-CTOR-PRIV-AT? / TF-CTOR-PRIV-WORD? (src/core/type-family.f ~1306-1324, landed by bb8224b3) walk every SUMV row of every private family of the open package without asking whether that family generates, so inside a package that declares a private 'SUMTYPE colour 0 VARIANT red … ;SUMTYPE' (which publishes no word: TDECL-GENERATES? answers TFAM-PRODUCT?) an ordinary ': COLOUR-RED ( -- n ) 5 ;' defines cleanly but 'undefine COLOUR-RED' throws 7111 E-CTOR-PROTECTED — a user-facing regression: the package silently loses the ability to undefine its own words spelled FAMILY-VARIANT, with an error naming a protection the word never had; before bb8224b3 a bare name could never match (TF-CW-SPLIT? required a colon). Acceptance: the private recognizer is gated on the same TDECL-GENERATES? predicate the generator uses and on the product member set (MAKE, UNMAKE, and the DERIVE addr members once habu-generate-typed-field-ba63866e lands: FIELD names, AT, BYTES, CELLS) rather than on variant names; a red-first case in test/structure-decl-suite.f (a private sum's variant-spelled word undefines; a private product's HIDDEN-MAKE still refuses 7111); docs/type-families.md's protection paragraph states the member set. Files: src/core/type-family.f, test/structure-decl-suite.f, docs/type-families.md. Verify: the suite on gen2; three generations with cmp (type-family.f is baked); tools/bootstrap.sh check; test/run.f. Depends: none. Ownership: type system. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
