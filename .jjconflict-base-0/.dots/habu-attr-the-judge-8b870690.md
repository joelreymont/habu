---
title: "Attribute the judge CLI's own failures"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T12:06:08.546050+02:00"
---

Found by the Wave-0/2 audit: bin/hb --load tools/judge.f -- --check and tools/judge-test.f die on an engine/tree mismatch with ONE uncaught line (-8713 E-JUDGE-CHAIN-DEP; codegen-compare with -8602) and no corpus/subject name - violating the boundary attribution policy they live under (docs/forth.md:1068), and E-JUDGE-CHAIN-DEP exists precisely to avoid misattribution. Catch at the CLI boundary, name the subject and the remedy (install --force - the stale-binary lesson), exit with the same code. Files: tools/judge.f, tools/judge-test.f, tools/codegen-compare.f. Depends: none.
