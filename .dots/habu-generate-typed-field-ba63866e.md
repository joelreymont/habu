---
title: Generate typed field accessors with DERIVE addr
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T10:24:05.091581+03:00"
---

Problem: field-project (src/core/checker.f FIELD-PROJ-STEP, armed by the sealed FIELD-PROJ!, pinned by test/field-proj-suite.f) is the checker half of a record field door that has no generator (tracker id habu-structure-generate-field-b9dc52f8, docs/type-families.md section 2.2), and its production runtime word is E-UNDEFINED; a layout pointer today is reachable only whole-value (@ then UNMAKE) and cannot be indexed because + refuses ptr F. Acceptance (docs/type-system.md section 10.4): DERIVE addr as a third derive code beside eq and hash in structure-decl.f's header clause; per field F:f ( ptr F -- ptr T ) with body <byte-offset> field-project, armed per accessor by FIELD-PROJ!, the committed field id the sole authority; F:AT ( ptr F n -- ptr F ) over a new pointee-preserving record-at checker row scaled by the committed width; F:BYTES and F:CELLS; the production field-project runtime word ( ptr a n -- ptr a ) + ; a pointer field projects as ptr ptr t and a generic field at the caller's instantiation; every negative in test/field-proj-suite.f still rejects when reached through a generated accessor; a field read through an accessor equals the same field read through UNMAKE for a pointer field, a scalar field and a nested family field. Files: src/core/structure-make.f (or a new structure-field.f), src/core/structure-decl.f, src/core/checker.f (record-at row), src/core/type-family.f, test/field-proj-suite.f. Verify: the suite; three generations with cmp; tools/bootstrap.sh check; test/run.f. Depends: habu-generate-a-private-80272413. Ownership: type system. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
