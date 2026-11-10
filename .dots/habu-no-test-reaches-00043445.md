---
title: "No test reaches tier 1's two top-level runtime-word bindings"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:26:10.106334+02:00"
---

Problem: src/compiler/ir/elaborate.f :3599 (QUOTATION-STORAGE:STORE) and :3615 (the fetch check) bind a runtime word through NDICT:TOP-TARGET since habu-a-pkg-part-3ae2acf8, and no test reaches either: tier 0 compiles program code, and these sites are tier 1's, which compiles Wasm modules only. The review's probe /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/join-review/p2/qs.f (a part QUOTATION-STORAGE whose STORE prints) never runs the part's STORE on tier 0; a Wasm build could not be run from a scratch tree (tools/wasm-build.f: include: cannot open tools/src/habu/prefix-rewind.f). Found by the review of habu-a-pkg-part-3ae2acf8 (claim 4). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: when Wasm compiles through tier 0 the two sites go with tier 1, or a Wasm case shows a part named QUOTATION-STORAGE or NFETCH-CHECK cannot stand in for the runtime word. Files: src/compiler/ir/elaborate.f, test/wasm. Verify: qs.f built for Wasm. Depends: habu-emit-wasm-from-0d5dab47. Ownership: the Wasm build. Claim: unassigned.
