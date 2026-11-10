---
title: Delete SCHEMA-N@, which nothing reads
status: active
priority: 4
issue-type: task
created-at: "2026-11-10T21:10:32.936374+02:00"
---

Problem: src/core/type-schema.f SCHEMA-N@ (the schema arena's node high-water) has no caller: the only mentions are its definition, its PPRIM: axiom (src/core/checker.f, PPRIM: SCHEMA-REG SCHEMA-N@) and two comments (type-schema.f 'Read the registry control cells through', checker.f 'The two high-water reads are the package's whole checked surface'). Its trailing comment says 'for rollback/tests'; no tool or library reads it, and the one test that names it, test/prop-test.f GENERIC-CASES (in lower case: search with rg -i), only checks that checked code may call it. SCHEMA-ROOT-N@ is read by src/core/type-family.f, sumtype.f, structure-make.f and structure-decl.f and stays. Acceptance: the word, its axiom, its prop-test case and the mentions are gone; the comments name SCHEMA-ROOT-N@ alone. Files: src/core/type-schema.f, src/core/checker.f, test/prop-test.f. Verify: engines converge; the suite. Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w4.
