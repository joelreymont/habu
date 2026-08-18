---
title: Reach the absent arity or retire its code
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T22:22:18.682814+02:00"
---

src/compiler/native/migrate.f KEEP-ARITY throws E-NMIGRATE-ARITY (-8579) when NDICT:SPELL-ARITY answers ARITY-NONE for the name the source just published. Handling the reader's documented absent answer is right - letting -1 through to NELAB:COLON would refuse it as E-NELAB-ARITY and name the wrong thing - but NO SHAPE REACHES IT today, measured through NMIGRATE:MEASURE-HELD on 2026-08-18: a package opened and closed by the source is E-NMIGRATE-NAME -8573, a TRUSTED: body is E-NFEED-STATE -8400, a 0-set-check region ends the process at exit 70, an unsigned body answers its inferred effect and compiles. E-NMIGRATE-VERDICT already refuses anything the engine's check did not certify, and KEEP-ARITY asks about a record published one step earlier in the scope that published it - so the argument that it is structurally unreachable is strong and untested. The census header (tools/chain-census-core.f) names a seal-stripped name as a word with no queryable effect; find whether such a name can reach the migration entry. Either land a test that reaches -8579 through the production entry, or retire the code and let the reader's contract be asserted where SPELL-ARITY is tested. A named refusal with no test is debt either way.
