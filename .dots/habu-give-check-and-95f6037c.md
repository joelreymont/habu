---
title: Give check and load one undefined-word record
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T05:39:29.915643+01:00"
---

Problem: a top-level word nothing defines gets two records. The load writes it in src/habu/interpreter.f:325 UNDEFINED-NAME (E-UNDEFINED, help REJECTION:UNDEFINED-HELP), check.f in src/core/check-walk.f:2334 TOP-REJECT (E-UNDEFINED-TOP-LEVEL, help render.f:1833 UNDEF-HELP). For /tmp/jerry-undef/a.f ('package P public : foo ( -- ) ; ;package' then a bare 'foo' at top level) the load's help says 'define `foo` before this point' and never names P:foo; check.f's help says '`foo` is the word this definition defines: call it from its own body with `recurse`', which is false at top level. Acceptance: one word writes the record for a top-level undefined word and both the load and check.f call it; for a.f both print the same record, named E-UNDEFINED, whose help names P:foo and 'using P'; the recurse help appears only inside the body of the definition being defined; docs/forth-card.md:221, docs/forth.md:96 and :2571 and tools/lsp-test-lib.f:129 and :2489 name E-UNDEFINED; a test runs a.f through both. Depends: none. Claim: unassigned.
