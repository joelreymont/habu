---
title: Turn the registry bridges into checker axioms
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T06:55:29.416999+02:00"
---

src/compiler/native/family.f carries ten one-line TRUSTED: bridges onto the type-family registry (TFL-MATCH-FAM? etc.) because the registry's readers live in the boot prefix where the seal strips their symbols - the same wall dict.f met. The structural replacement is one PRIM: row per word in src/core/checker.f, which is a boot-prefix change under the two-stage rule plus the bootstrap/cg/forth.fs mirror. Do dict.f's boundary in the same pass. Acceptance: the TRUSTED: rows in family.f and dict.f deleted; the PRIM rows assert the same effects; bootstrap mirror lint green; two-stage landing per docs/bootstrap.md. Files: src/core/checker.f, bootstrap/cg/forth.fs, src/compiler/native/{family,dict}.f. Depends: none (schedule with any other boot-prefix change to amortise the two-stage cost).
