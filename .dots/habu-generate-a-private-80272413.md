---
title: "Generate a private family's words into its wordlist"
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T10:24:05.089089+03:00"
---

Problem: src/core/structure-decl.f gates MAKE/UNMAKE generation on a public family (package-scoped private generation is deferred type-DSL work), so a private STRUCTURE loads and stores but generates nothing; most memory records in the tree are package-private (lib/byte-buffer.f BUF, lib/json-read.f JR, lib/xml/state.f XML, lib/byte-edit.f EDIT, tools/lint/text.f LINT-SLAB), so the declared memory record of docs/type-system.md section 10 serves nothing in lib/ without it. Acceptance: a private STRUCTURE in a package publishes its generated words (constructors now, the DERIVE addr accessors once the generator lands) into the package's private wordlist inside the declaration transaction; a second package cannot resolve them; a rejected declaration rolls back byte-identically; fixtures in test/structure-decl-suite.f. Files: src/core/structure-decl.f, src/core/structure-make.f, test/structure-decl-suite.f. Verify: the suite; three generations with cmp (prefix files are baked); tools/bootstrap.sh check; test/run.f. Depends: none. Ownership: type system. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
