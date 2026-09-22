---
title: Reach the ADR a stripped Tender image cannot
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T07:15:31.649939+03:00"
---

Problem: the same measurement as habu-name-the-site-03a33e54 - the standalone tender image's closure is large enough that some ADR (reach +-1 MiB, src/arch/arm64/icode.f ADR-LO/ADR-HI) cannot reach its target: a captured body's literal or data reference, or an engine helper's, placed beyond the window by the stripped layout (src/habu/aot-lib.f). The AOT-section precedent (dot d1326596: 29 sites at chain scale, fixed by TADR, and kept by tools/aot-section-reach-lint.f) shows the family. Acceptance: with the site named (habu-name-the-site-03a33e54), decide whether the layout keeps the target in reach (bands adjacent to the code that addresses them) or the reference addresses long (ADRP+ADD / TADR,), weighing thin binaries and code size; implement it; the tender standalone build completes (aspen measures); a fixture builds a stripped image past the ADR reach and pins it, or the measured reason such a fixture is infeasible at test scale is recorded beside the code. Files: src/habu/aot-lib.f, src/arch/arm64/icode.f, src/compiler/native emitters as found, tools/hb-build-test.f. Verify: tools/hb-build-test.f; test/run.f; Tender's build with aspen. Depends: habu-name-the-site-03a33e54. Ownership: hazel. Claim: unassigned.
