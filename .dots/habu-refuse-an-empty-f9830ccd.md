---
title: Refuse an empty root by name
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:53:28.731382+02:00"
---

Problem: SOURCE-ROOT:ROOT! with an empty path throws E-PATH-RANGE, a bare code, where ROOT!'s other refusals say what was wrong; SOURCE-ROOT:WITH with an empty path throws the same bare code (ROOT-CANON, ABSOLUTE!, CHECK). ROOT!'s three 'source root: path is too long' refusals say what was wrong and name no path (feature review of habu-keep-active-src-2da0b7b5; read from the code, not run). Acceptance: an empty path is refused, by ROOT! and by WITH, with a line that says the root is empty. Files: src/core/include.f, test/source-root-test.f. Verify: the case in test/source-root-test.f. Depends: none. Ownership: none. Claim: unassigned.
