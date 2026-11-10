---
title: "C4: a build's declarations write the product's records"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:30:41.274095+02:00"
---

Problem: design r2 D4 (/tmp/jerry-big/design-facts-r2.md): the product's declaration words run as product code inside a build and record into the product's own tables, which hb's checker never reads (/tmp/jerry-ckrows/census4.md classes A and D: TRUST asks the package mirror, INTRINSIC asks CK-CLOSED-SYM, names: asks TOP-BINDING, TYPED-VARIABLE arms LAYOUT-INTRO, STRUCTURE's span registers in the product's registry). Acceptance: every declaration writer finds its record through the dictionary and writes the product's records' facts with CHECK:FACTS!; LAYOUT-INTRO is replaced; a build loads the startup rows past row 68 with hb's checker; census classes A and D closed (B and C stay S5c's). The product's created words keep [32] = 0 until C5 (nothing lays their created text before the linker's prelude); hb's per-build stores carry them. Files: src/core/checker.f, src/core/type-family.f, the definers. Verify: the linker test; bin/hb --load test/run.f. Depends: C3. Ownership: those files. Claim: unassigned.
