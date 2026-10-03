---
title: Refuse linear names any family or lexer claims
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:07:52.662987+02:00\""
closed-at: "2026-10-01T17:39:58.439495+02:00"
close-reason: Fixed by vtqyrusn e3140a42 (review 177 ACCEPT)
---

Problem (review 80 of 7498dc6d, evidence ~/.cache/tmp/kestrel-r4-rev80/cases/): TYPE-RESERVED? (src/core/checker.f:4566-4578) asks SIG-FAM? with the declaring scope (:4466), and TFAM-SIG-RESOLVE maps E-TFAM-AMBIG to not-found (src/core/type-family.f:4292). So a top-level DEFLINEAR foo is admitted when FOO is a family private to package P, or a public tail two packages share; an effect inside P then reads foo as the family and drops a linear (fam-scope-pkg.f, fam-scope-ambig.f: load rc 0, check.f rc 0). docs/effects.md:237-242 lists only part of the rule (not a family, a VALUE-RECORD name, an atom prefix, or field). check.f gates the name on the lint lexer's token kind first (tools/check-core.f:689-691, :777), so DEFLINEAR ( and DEFLINEAR s" load rc 0 but check.f refuses. Acceptance: a declared linear or VALUE-RECORD name that any package's family tail claims is refused by the loader and check.f alike (registry-wide tail lookup, the mirror of DEFTYPE's reserved-name refusal); the two lexer-syntax shapes are refused by the shared rule; effects.md states the whole rule; the reviewer's cases become check-test cases that fail first. Files: src/core/checker.f, src/core/type-family.f, tools/check-core.f, tools/check-test-lib.f, docs/effects.md. Verify: tools/check-test.f, engine rebuild g1=g2, two-generation build.
