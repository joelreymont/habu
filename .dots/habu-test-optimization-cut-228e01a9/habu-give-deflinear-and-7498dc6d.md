---
title: Give DEFLINEAR and VALUE-RECORD one name rule
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T04:59:43.092872+02:00"
---

Problem: tools/check-core.f:772 CHK-NOM-NAME-BAD? runs TYPE-RESERVED? on the lowercased name for DEFLINEAR and VALUE-RECORD, but their loaders (CT-ADD-LINEAR, CHECKER-DEFRECORD) test the name as written, so `DEFLINEAR N`, `DEFLINEAR CELL` and `VALUE-RECORD PTR x n END-VALUE-RECORD` load rc 0 and check rc 70 (measured by the r4-tic6x lane on qqrsrlsv; DEFTYPE was fixed there by making check.f call the loader's CHECKER-DEFFAMILY). Separately, lib/type/deftype.f reports any CHECKER-DEFFAMILY failure as a duplicate family and check.f reports it as E-BAD-NOMINAL-TYPE, whatever the cause. Acceptance: measure how a stack-effect type token resolves case; choose the rule that refuses every name that would collide with a reserved or existing type there; make the loaders and check.f apply that one rule through one shared word, so they cannot drift; fix any tree source the stricter rule refuses; if a non-name failure (registry capacity) is reachable, both report it as itself. Cases through tools/check-test-lib.f, written first and seen to fail, for each spelling above and a legal name. Files: tools/check-core.f, tools/check-test-lib.f, the DEFLINEAR / VALUE-RECORD / DEFTYPE loaders as the rule needs. Verify: tools/check-test.f, test/gate-diagnostics.f, the loaders' suites. Depends: habu-check-tic6x-sim-a4e09b51 (r4-tic6x). Ownership: nominal-type name rules.
