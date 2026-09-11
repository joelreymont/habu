---
title: Make spill rewrite and combine linear
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.270563+03:00"
---

Problem: A64SPILL:REWRITE 2.7 s at exponent 1.94 and COMBINE 20.3 s at ops^1.57 (1.21 in the tail); a trivial body with one rewrite pays 2.4 ms in COMBINE because a rewrite rebuilds the module. Acceptance: both fit slopes at most 1.1; a combine rewrite edits in place or rebuilds once per definition; controlled pair. Files: src/compiler/native/spill.f, src/compiler/native/combine.f. Verify: the native suites; tender-perdef scaling fit. Depends: none. Ownership: cedar (combiner) or grant. Claim: unassigned
