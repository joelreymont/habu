---
title: Admit a tier-0 does> beside unused head locals
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:43.056468+03:00"
---

Problem: tier 0 refuses rc 75 a definer whose does> clause has a head local in scope (src/habu/habu2.f J-DOES locals check, about :4527 on 2026-10-10), even when the clause names no head local; tier 1 and the language admit it. docs/debugging.md:40 and src/lib/codegen.f:57-59 describe the tier-0 refusal. Reproduce first at both tiers on ARM64.
Acceptance: tier 0 admits such a definer and runs it as tier 1 does; a clause that does name a head local is refused at both tiers as the language requires; the two docs passages match. Verify: rebuild bin/hb per docs/gate.md; the reproducer as a native-suite row; `bin/hb --load test/run.f`; two-generation build. Files: src/habu/habu2.f, bootstrap/cg/forth.fs where the seed mirrors the path, the test row. Claim: unassigned.
