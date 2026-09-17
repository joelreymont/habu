---
title: Convert the raw pointer cells in src/core, src/os and src/habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T18:44:23.880007+03:00"
---

Problem: the raw-storage census (/home/joel/.cache/hazel/scout-pun/census.md) lists the engine-source sites the rule habu-refuse-a-ptr-5ad2734e refuses under src/core, src/os and src/habu (src/core/cell-effects.f:39 INSTALL first, src/habu/verify-source.f 17, aot-decl.f, aot-closure.f, include.f, sha256.f, env-base.f, image-bytes.f), plus what it could not measure: src/core/checker.f's own ~52 ptr-field sites (the host compiles checker.f before the hook installs, so they refuse only when a rule engine builds the next generation) and top-level code outside definitions (src/core/checker.f:127 DECLARATIONS data-base TARGET-CELL + 0 ptr-field !; grep '^[^:].*ptr-field'). Acceptance: every mechanical-shape site in those directories converted to the declared form the census names; checker.f's sites and the top-level sites converted by inspection; the converted tree's engine prefix loads on ~/.cache/hazel/engines/raw-rule-gen1 as far as the prefix order allows and the engine builds on the release engine to a byte fixpoint; test/run.f green. Files: src/core/, src/os/, src/habu/ per the census. Verify: raw-rule-gen1 loads; fixpoint; tools/bootstrap.sh check when the prefix files change; test/run.f. Depends: habu-fix-the-definers (the definer fix removes the generated-accessor rows first). Ownership: engine sources. Parent: habu-refuse-a-ptr-5ad2734e. Claim: unassigned.
